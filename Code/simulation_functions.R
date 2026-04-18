# =============================================================================
# simulation_functions.R
# BNE scoring auction simulation: NJ ordinal vs MI cardinal cost scoring
#
# Model:
#   - 1 district, 2 firms, 1 auction
#   - Firm i has private cost c_i ~ Uniform[cLi, cHi] and known quality q_i
#     (quality fixed and known to both firms — extend to private quality later)
#   - District posts (w, scoring rule) before bidding
#   - Firms simultaneously submit bids b_i >= c_i
#   - District scores and awards to highest total score
#   - Winning firm earns profit b_i - c_i
#
# Scoring rules:
#   Ordinal (NJ):   cost_score_i = S if rank 1, S-1 if rank 2, etc.
#   Cardinal (MI):  cost_score_i = max(0, (1 - (b_i - b_min)/b_min) * S)
#
# Total score: w * cost_score_i + (1 - w) * q_i
#
# TODO: extend to private quality (firms have prior over q_j); N > 2 firms
# =============================================================================

# ── 1. Single auction outcome ─────────────────────────────────────────────────

#' Determine winner given two scalar bids
#'
#' @param b1,b2   scalar bids (must be >= 0)
#' @param q1,q2   scalar quality types on [0, S]
#' @param w       district's scoring weight on cost in [0, 1]
#' @param S       max cost score points (default 5, matching NJ Form 320A)
#' @param rule    "ordinal" or "cardinal"
#' @return list(winner = 1|2, price = winning bid, quality = winner quality)
auction_outcome <- function(b1, b2, q1, q2, w, S = 5, rule) {
  if (rule == "ordinal") {
    # Lowest bid gets S points; 2nd lowest gets S-1 points
    # Ties broken in favor of firm 1 (prob-0 event with continuous types)
    cs1 <- if (b1 <= b2) S else S - 1L
    cs2 <- if (b2 <  b1) S else S - 1L
  } else {
    # Cardinal: lowest bid gets S; others scaled by proportional overage
    # Formula: (1 - (b_i - b_min)/b_min) * S, floored at 0
    bmin <- min(b1, b2)
    cs1  <- max(0, (1 - (b1 - bmin) / bmin) * S)
    cs2  <- max(0, (1 - (b2 - bmin) / bmin) * S)
  }
  ts1 <- w * cs1 + (1 - w) * q1
  ts2 <- w * cs2 + (1 - w) * q2
  if (ts1 >= ts2) list(winner = 1L, price = b1, quality = q1)
  else            list(winner = 2L, price = b2, quality = q2)
}


# ── 2. Win probability (vectorized over competitor's bid function) ─────────────

#' Prob(firm 1 wins) at scalar bid b, integrated over firm 2's type distribution
#'
#' Competitor's bid function is supplied as a vector beta2 evaluated on c2_grid.
#' Since c2 ~ Uniform, E = mean over the grid.
#'
#' Ordinal shortcut: win conditions A1 (wins with cost rank 1) and A2 (wins with
#' rank 2) are pure functions of (w, q1, q2, S) — independent of bid magnitudes.
#' This avoids inner loops in the ordinal case.
#'
#' @param b      scalar bid for firm 1
#' @param q1,q2  quality types
#' @param beta2  competitor's bid function evaluated on its cost grid (vector)
#' @param w,S    scoring parameters
#' @param rule   "ordinal" or "cardinal"
win_prob_1 <- function(b, q1, q2, beta2, w, S = 5, rule) {
  if (rule == "ordinal") {
    # A1: firm 1 wins if it gets cost rank 1 (b < bj)
    A1 <- (w * S       + (1 - w) * q1) > (w * (S - 1) + (1 - w) * q2)
    # A2: firm 1 wins even with cost rank 2 (b > bj) — requires large quality gap
    A2 <- (w * (S - 1) + (1 - w) * q1) > (w * S       + (1 - w) * q2)
    mean((b < beta2) * A1 + (b > beta2) * A2)

  } else {
    # Cardinal: win condition depends on actual bid magnitudes
    bmin <- pmin(b, beta2)           # element-wise min (b is scalar, beta2 vector)
    cs1  <- pmax(0, (1 - (b      - bmin) / bmin) * S)
    cs2  <- pmax(0, (1 - (beta2  - bmin) / bmin) * S)
    ts1  <- w * cs1 + (1 - w) * q1
    ts2  <- w * cs2 + (1 - w) * q2
    mean(ts1 >= ts2)
  }
}

# Symmetric version for firm 2
win_prob_2 <- function(b, q2, q1, beta1, w, S = 5, rule) {
  win_prob_1(b, q2, q1, beta1, w, S, rule)
}


# ── 3. Best response ──────────────────────────────────────────────────────────

#' Firm 1's profit-maximizing bid at cost c1, given firm 2's bid function beta2
#'
#' Grid search over [c1, b_max]. Increasing n_search improves accuracy.
#'
#' @return scalar optimal bid
br1 <- function(c1, q1, q2, beta2, w, S, rule, b_max, n_search = 150) {
  bs <- seq(c1, b_max, length.out = n_search)
  pi <- (bs - c1) * vapply(bs, win_prob_1, numeric(1),
                            q1 = q1, q2 = q2, beta2 = beta2,
                            w = w, S = S, rule = rule)
  bs[which.max(pi)]
}

#' Firm 2's profit-maximizing bid at cost c2
br2 <- function(c2, q2, q1, beta1, w, S, rule, b_max, n_search = 150) {
  bs <- seq(c2, b_max, length.out = n_search)
  pi <- (bs - c2) * vapply(bs, win_prob_2, numeric(1),
                            q2 = q2, q1 = q1, beta1 = beta1,
                            w = w, S = S, rule = rule)
  bs[which.max(pi)]
}


# ── 4. BNE solver: iterative best response ────────────────────────────────────

#' Solve for BNE bid functions via iterative best response
#'
#' Initializes at truthful bidding (b_i = c_i) and iterates until convergence.
#' For the ordinal rule, pre-checks whether the competition is trivial
#' (one firm always wins) and shortcuts accordingly.
#'
#' @param q1,q2         known quality types (on [0, S] scale)
#' @param cL1,cH1       firm 1 cost support (Uniform distribution)
#' @param cL2,cH2       firm 2 cost support
#' @param w             scoring weight on cost
#' @param S             max cost score points
#' @param rule          "ordinal" or "cardinal"
#' @param n_grid        number of grid points per firm's cost space
#' @param n_search      bid grid resolution for best-response search
#' @param max_iter      max iterations
#' @param tol           convergence tolerance (sup-norm on bid function change)
#' @param reserve_price district's max willingness to pay (caps monopoly bids)
#' @return list(c1, b1, c2, b2, converged, iters, regime)
solve_bne <- function(q1, q2, cL1, cH1, cL2, cH2, w, S = 5, rule,
                       n_grid     = 40,
                       n_search   = 150,
                       max_iter   = 60,
                       tol        = 1e-3,
                       reserve_price = NULL) {

  stopifnot(cL1 < cH1, cL2 < cH2, w >= 0, w <= 1, S > 0)

  c1    <- seq(cL1, cH1, length.out = n_grid)
  c2    <- seq(cL2, cH2, length.out = n_grid)
  b_max <- if (!is.null(reserve_price)) reserve_price else max(cH1, cH2) * 2

  # ── Ordinal shortcut: check whether competition is trivial ──────────────────
  if (rule == "ordinal") {
    A1 <- (w * S       + (1 - w) * q1) > (w * (S - 1) + (1 - w) * q2)
    A2 <- (w * (S - 1) + (1 - w) * q1) > (w * S       + (1 - w) * q2)

    if (!A1 && !A2) {
      # Firm 1 can never win: firm 2 wins regardless of bids.
      # Firm 2 acts as monopolist (bids up to reserve); firm 1 bids at cost.
      b2_mono <- pmin(b_max, c2 + (b_max - c2) * 0.9)
      return(list(c1 = c1, b1 = c1, c2 = c2, b2 = b2_mono,
                  converged = TRUE, iters = 0L, regime = "firm2_monopoly"))
    }

    if (A1 && A2) {
      # Firm 1 always wins: acts as monopolist; firm 2 bids at cost.
      b1_mono <- pmin(b_max, c1 + (b_max - c1) * 0.9)
      return(list(c1 = c1, b1 = b1_mono, c2 = c2, b2 = c2,
                  converged = TRUE, iters = 0L, regime = "firm1_monopoly"))
    }
    # A1 = TRUE, A2 = FALSE: competitive regime (both need cost rank to win)
    regime <- "competitive"
  } else {
    regime <- "competitive"
  }

  # ── Iterative best response ──────────────────────────────────────────────────
  b1 <- c1   # init: bid at cost
  b2 <- c2

  for (iter in seq_len(max_iter)) {
    b1_old <- b1
    b2_old <- b2

    b1 <- vapply(c1, br1, numeric(1),
                  q1 = q1, q2 = q2, beta2 = b2,
                  w = w, S = S, rule = rule, b_max = b_max, n_search = n_search)

    b2 <- vapply(c2, br2, numeric(1),
                  q2 = q2, q1 = q1, beta1 = b1,
                  w = w, S = S, rule = rule, b_max = b_max, n_search = n_search)

    delta <- max(max(abs(b1 - b1_old)), max(abs(b2 - b2_old)))
    if (delta < tol) break
  }

  list(c1 = c1, b1 = b1, c2 = c2, b2 = b2,
       converged = (iter < max_iter), iters = iter, regime = regime)
}


# ── 5. District welfare ───────────────────────────────────────────────────────

#' Expected district utility over the joint type distribution
#'
#' Integrates auction_outcome over (c1, c2) ~ Uniform x Uniform via the grid.
#' District true utility: (1 - lambda) * quality - lambda * price
#'
#' Note on scaling: quality is on [0, S]; price is in cost units.
#' lambda controls the relative valuation: lambda = 0.5 means 1 quality unit
#' equals 1 cost unit in welfare terms.
#'
#' @param bne    output of solve_bne()
#' @param lambda district's true weight on cost (higher = cares more about price)
compute_welfare <- function(bne, q1, q2, lambda, w, S = 5, rule) {
  utils <- outer(seq_along(bne$c1), seq_along(bne$c2), Vectorize(function(i, j) {
    out <- auction_outcome(bne$b1[i], bne$b2[j], q1, q2, w, S, rule)
    (1 - lambda) * out$quality - lambda * out$price
  }))
  mean(utils)
}

#' Welfare as a function of scoring weight w (the district's choice variable)
#'
#' This is the key quantity: for each scoring rule, the district optimizes over w.
#' The gap between the two rules' optima is the welfare cost of ordinal scoring.
#'
#' @param n_w       number of w values to evaluate (more = smoother curve)
#' @param n_grid    BNE type grid resolution (lower = faster)
#' @param n_search  best-response bid search resolution
#' @return tibble with columns: w, welfare, rule
welfare_over_w <- function(q1, q2, cL1, cH1, cL2, cH2, lambda, S = 5, rule,
                            n_w = 15, n_grid = 30, n_search = 100) {
  w_seq <- seq(0.05, 0.95, length.out = n_w)

  welf <- vapply(w_seq, function(w) {
    bne <- solve_bne(q1, q2, cL1, cH1, cL2, cH2, w, S, rule,
                     n_grid = n_grid, n_search = n_search,
                     max_iter = 50, tol = 1e-3)
    compute_welfare(bne, q1, q2, lambda, w, S, rule)
  }, numeric(1))

  data.frame(w = w_seq, welfare = welf, rule = rule,
             stringsAsFactors = FALSE)
}
