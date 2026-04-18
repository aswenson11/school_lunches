# =============================================================================
# app.R
# Shiny interface for the FSMC procurement auction simulator
#
# Tabs:
#   1. Bid Functions  — BNE β*(c) for both rules at the current w slider
#   2. Welfare Curves — E[U(w)] for both rules; shows optimal w* under each
#   3. Summary        — welfare loss table + plain-language interpretation
#
# Usage: shiny::runApp("Code/app.R") from project root, or open in RStudio
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

source(file.path(dirname(rstudioapi::getSourceEditorContext()$path),
                 "simulation_functions.R"),
       local = TRUE)

# Fallback if not running from RStudio
if (!exists("auction_outcome")) {
  source("simulation_functions.R")
}


# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "FSMC Procurement Auction Simulator",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  sidebar = sidebar(
    width = 290,

    # ── Firm parameters ──────────────────────────────────────
    h6("Firm quality  (scale: 0 – S points)", class = "text-muted mt-2"),
    layout_columns(
      col_widths = c(6, 6),
      sliderInput("q1", "q₁ (Firm 1)", min = 0, max = 5, value = 3.5, step = 0.25),
      sliderInput("q2", "q₂ (Firm 2)", min = 0, max = 5, value = 2.0, step = 0.25)
    ),

    # ── Cost distributions ───────────────────────────────────
    hr(),
    h6("Cost distributions  (Uniform)", class = "text-muted"),
    p("Firm 1  c₁ ~ U[cL₁, cH₁]", style = "font-size:0.82em; margin-bottom:2px"),
    sliderInput("c1_range", NULL, min = 0.5, max = 12, value = c(2, 4),   step = 0.25),
    p("Firm 2  c₂ ~ U[cL₂, cH₂]", style = "font-size:0.82em; margin-bottom:2px"),
    sliderInput("c2_range", NULL, min = 0.5, max = 12, value = c(2.5, 5), step = 0.25),

    # ── District ─────────────────────────────────────────────
    hr(),
    h6("District", class = "text-muted"),
    sliderInput("lambda", "True cost weight  λ",
                min = 0, max = 1, value = 0.5, step = 0.05),
    helpText("U = (1−λ)·quality − λ·price"),

    sliderInput("w_disp", "Scoring weight w  (bid-function plot)",
                min = 0.05, max = 0.95, value = 0.5, step = 0.05),

    # ── Scoring ──────────────────────────────────────────────
    hr(),
    h6("Scoring", class = "text-muted"),
    numericInput("S", "Max cost score points (S)",
                 value = 5, min = 1, max = 20, step = 1),
    helpText("NJ Form 320A uses S = 5; adjust to explore sensitivity."),

    # ── Reference-price cardinal ─────────────────────────────
    sliderInput("p_ref", "Reference price  p_ref  (ref-price rule)",
                min = 0.5, max = 12, value = 3.4, step = 0.05),
    helpText("Ref-price cost score = S·(1 \u2212 (b \u2212 p_ref)/p_ref), floored at 0. ",
             "Default = midpoint of the two firms' prior midpoints (auto-syncs)."),

    # ── Run welfare ──────────────────────────────────────────
    hr(),
    actionButton("run_welfare", "Compute welfare curves",
                  class = "btn-primary w-100",
                  icon  = icon("chart-line")),
    helpText("Solves BNE across all w values for both rules. ~10–30 seconds.")
  ),

  # ── Main panels ──────────────────────────────────────────────────────────────
  navset_card_tab(

    # Tab 1: Bid functions
    nav_panel(
      title = "Bid Functions",
      icon  = icon("arrows-up-down"),
      plotOutput("plot_bids", height = "460px"),
      card_footer(
        tags$em(
          "Equilibrium bid functions \u03b2*(c) for both scoring rules at the current w,",
          " shown separately for each firm.",
          " Dashed line = bid equals cost (zero markup).",
          " Bids above the dashed line are equilibrium markups — firms shade their bids above",
          " private cost to earn positive expected profit, exactly as in standard procurement auctions."
        )
      )
    ),

    # Tab 2: Welfare curves
    nav_panel(
      title = "Welfare Curves",
      icon  = icon("chart-line"),
      plotOutput("plot_welfare", height = "460px"),
      card_footer(
        tags$em(
          "E[U(w)] = expected district utility as a function of scoring weight w.",
          " Triangles mark each rule's optimal w*.",
          " The vertical gap between the two peaks is the welfare cost of ordinal scoring."
        )
      )
    ),

    # Tab 3: Summary
    nav_panel(
      title = "Summary",
      icon  = icon("table"),
      br(),
      uiOutput("ui_summary")
    )
  )
)


# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Input validation ────────────────────────────────────────────────────────
  observe({
    if (isTruthy(input$c1_range) && input$c1_range[1] >= input$c1_range[2])
      showNotification("Firm 1: cL must be strictly less than cH", type = "warning")
    if (isTruthy(input$c2_range) && input$c2_range[1] >= input$c2_range[2])
      showNotification("Firm 2: cL must be strictly less than cH", type = "warning")
  })

  # ── Auto-sync p_ref to the midpoint of the two priors' midpoints ─────────────
  # Fires whenever the cost ranges change. Users can still override by dragging.
  observeEvent(list(input$c1_range, input$c2_range), {
    req(input$c1_range, input$c2_range)
    mid1 <- mean(input$c1_range)
    mid2 <- mean(input$c2_range)
    p_ref_default <- round((mid1 + mid2) / 2, 2)
    updateSliderInput(session, "p_ref", value = p_ref_default)
  }, ignoreInit = FALSE)

  # ── BNE at current w_disp — all three rules (for bid function plot) ─────────
  #
  # n_grid = 35, n_search = 120: ~3–6s per rule. bindCache prevents
  # recomputation when only lambda or run_welfare changes.
  bne_both <- reactive({
    req(
      input$c1_range[1] < input$c1_range[2],
      input$c2_range[1] < input$c2_range[2],
      input$p_ref > 0
    )
    withProgress(message = "Solving BNE…", value = 0.2, {
      ord <- solve_bne(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$w_disp, input$S, rule = "ordinal",
        n_grid = 35, n_search = 120, max_iter = 50
      )
      setProgress(0.5)
      car <- solve_bne(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$w_disp, input$S, rule = "cardinal",
        n_grid = 35, n_search = 120, max_iter = 50
      )
      setProgress(0.8)
      car_ref <- solve_bne(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$w_disp, input$S, rule = "cardinal_ref",
        n_grid = 35, n_search = 120, max_iter = 50,
        p_ref = input$p_ref
      )
      setProgress(1.0)
      list(ordinal = ord, cardinal = car, cardinal_ref = car_ref)
    })
  }) |> bindCache(
    input$q1, input$q2,
    input$c1_range, input$c2_range,
    input$w_disp, input$S, input$p_ref
  )

  # ── Welfare curves (button-triggered, slower) ────────────────────────────────
  welfare_rv <- eventReactive(input$run_welfare, {
    req(
      input$c1_range[1] < input$c1_range[2],
      input$c2_range[1] < input$c2_range[2],
      input$p_ref > 0
    )
    withProgress(message = "Computing welfare curves…", value = 0.05, {
      setProgress(0.1, detail = "Ordinal rule (NJ)…")
      ord <- welfare_over_w(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$lambda, input$S, rule = "ordinal",
        n_w = 13, n_grid = 28, n_search = 90
      )
      setProgress(0.4, detail = "Cardinal rule (MI)…")
      car <- welfare_over_w(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$lambda, input$S, rule = "cardinal",
        n_w = 13, n_grid = 28, n_search = 90
      )
      setProgress(0.7, detail = "Reference-price cardinal…")
      car_ref <- welfare_over_w(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$lambda, input$S, rule = "cardinal_ref",
        n_w = 13, n_grid = 28, n_search = 90,
        p_ref = input$p_ref
      )
      setProgress(1.0)
      rbind(ord, car, car_ref)
    })
  })

  # ── Plot: bid functions ──────────────────────────────────────────────────────
  output$plot_bids <- renderPlot({
    bne <- bne_both()

    ref_lbl <- sprintf("Cardinal ref-price (p_ref = %.2f)", input$p_ref)

    df <- rbind(
      data.frame(firm = "Firm 1", rule = "Ordinal (NJ)",
                 cost = bne$ordinal$c1,  bid = bne$ordinal$b1),
      data.frame(firm = "Firm 1", rule = "Cardinal (MI)",
                 cost = bne$cardinal$c1, bid = bne$cardinal$b1),
      data.frame(firm = "Firm 1", rule = ref_lbl,
                 cost = bne$cardinal_ref$c1, bid = bne$cardinal_ref$b1),
      data.frame(firm = "Firm 2", rule = "Ordinal (NJ)",
                 cost = bne$ordinal$c2,  bid = bne$ordinal$b2),
      data.frame(firm = "Firm 2", rule = "Cardinal (MI)",
                 cost = bne$cardinal$c2, bid = bne$cardinal$b2),
      data.frame(firm = "Firm 2", rule = ref_lbl,
                 cost = bne$cardinal_ref$c2, bid = bne$cardinal_ref$b2)
    )

    # Lock rule ordering so legend and palette are consistent across plots
    df$rule <- factor(df$rule,
                      levels = c("Ordinal (NJ)", "Cardinal (MI)", ref_lbl))

    rule_colors <- setNames(
      c("#8C1515", "#0098DB", "#B26F16"),
      c("Ordinal (NJ)", "Cardinal (MI)", ref_lbl)
    )

    # Axis starts at 0 so the bid = cost reference line is a true 45-degree diagonal
    xy_max <- max(df$bid, df$cost) * 1.05

    ggplot(df, aes(x = cost, y = bid, color = rule)) +
      geom_abline(slope = 1, intercept = 0,
                  color = "grey65", linetype = "dashed", linewidth = 0.7) +
      geom_vline(xintercept = input$p_ref,
                 color = "#B26F16", linetype = "dotted", linewidth = 0.7, alpha = 0.6) +
      geom_line(linewidth = 1.3) +
      facet_wrap(~ firm) +
      coord_cartesian(xlim = c(0, xy_max), ylim = c(0, xy_max)) +
      scale_color_manual(values = rule_colors) +
      labs(
        title   = sprintf("Equilibrium bid functions    w = %.2f  |  q\u2081 = %.2f  q\u2082 = %.2f",
                           input$w_disp, input$q1, input$q2),
        x       = "True cost  c",
        y       = "Equilibrium bid  \u03b2*(c)",
        color   = "Scoring rule",
        caption = paste(
          "Dashed grey line: bid = cost (zero markup); bids above are equilibrium markups.",
          "Orange dotted line: reference price p_ref (bids at p_ref score S under the ref-price rule)."
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title   = element_text(size = 13),
            plot.caption = element_text(size = 10, color = "grey45", hjust = 0))
  })

  # ── Plot: welfare curves ─────────────────────────────────────────────────────
  output$plot_welfare <- renderPlot({
    req(welfare_rv())
    df <- welfare_rv()

    optima <- df |>
      group_by(rule) |>
      filter(welfare == max(welfare)) |>
      slice(1) |>
      ungroup()

    rule_labels <- c(
      "ordinal"      = "Ordinal (NJ)",
      "cardinal"     = "Cardinal (MI)",
      "cardinal_ref" = sprintf("Cardinal ref-price (p_ref = %.2f)", input$p_ref)
    )
    rule_colors <- c(
      "ordinal"      = "#8C1515",
      "cardinal"     = "#0098DB",
      "cardinal_ref" = "#B26F16"
    )

    ggplot(df, aes(x = w, y = welfare, color = rule)) +
      geom_line(linewidth = 1.3) +
      geom_vline(data = optima,
                  aes(xintercept = w, color = rule),
                  linetype = "dashed", alpha = 0.55) +
      geom_point(data = optima,
                  aes(x = w, y = welfare),
                  shape = 17, size = 4.5) +
      geom_vline(xintercept = input$lambda,
                  color = "grey40", linetype = "dotted", linewidth = 0.9) +
      annotate("text", x = input$lambda + 0.02, y = -Inf,
               label = paste0("\u03bb = ", input$lambda),
               hjust = 0, vjust = -0.5, size = 3.8, color = "grey40") +
      scale_color_manual(values = rule_colors, labels = rule_labels) +
      labs(
        title  = sprintf("District welfare vs. scoring weight    \u03bb = %.2f", input$lambda),
        x      = "Scoring weight on cost  w",
        y      = "Expected district utility  E[U]",
        color  = "Scoring rule"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # ── Summary tab ──────────────────────────────────────────────────────────────
  output$ui_summary <- renderUI({
    if (is.null(welfare_rv())) {
      return(div(
        class = "alert alert-secondary m-3",
        icon("info-circle"), " Click ",
        tags$strong("Compute welfare curves"), " to populate this tab."
      ))
    }

    df <- welfare_rv()
    optima <- df |>
      group_by(rule) |>
      filter(welfare == max(welfare)) |>
      slice(1) |>
      ungroup()

    w_ord      <- round(filter(optima, rule == "ordinal")$w,           3)
    w_car      <- round(filter(optima, rule == "cardinal")$w,          3)
    w_ref      <- round(filter(optima, rule == "cardinal_ref")$w,      3)
    eu_ord     <- round(filter(optima, rule == "ordinal")$welfare,     4)
    eu_car     <- round(filter(optima, rule == "cardinal")$welfare,    4)
    eu_ref     <- round(filter(optima, rule == "cardinal_ref")$welfare, 4)
    best_eu    <- max(eu_ord, eu_car, eu_ref)
    loss_ord   <- round(best_eu - eu_ord, 4)
    loss_car   <- round(best_eu - eu_car, 4)
    loss_ref   <- round(best_eu - eu_ref, 4)

    # Interpretation message
    w_dev  <- abs(w_ord - input$lambda)
    interp <- if (w_dev > 0.10) {
      sprintf(
        "Ordinal scoring forces the district to set w* = %.2f to compensate for
        rank-based distortion, a gap of %.2f from its true preference \u03bb = %.2f.
        Standard cardinal gives w* = %.2f; reference-price cardinal (p_ref = %.2f)
        gives w* = %.2f. The welfare gap between ordinal and the best-performing
        rule is %.4f utility units.",
        w_ord, w_dev, input$lambda, w_car, input$p_ref, w_ref, loss_ord
      )
    } else {
      sprintf(
        "Under these parameters the three rules produce similar optimal weights
        (ordinal w* = %.2f, cardinal w* = %.2f, ref-price w* = %.2f at p_ref = %.2f).
        Try widening the spread between the two firms' cost distributions, or
        increasing the quality gap, to amplify the distortion from ordinal scoring.",
        w_ord, w_car, w_ref, input$p_ref
      )
    }

    tagList(
      div(class = "m-3",
        h5("Optimal scoring weights and welfare"),
        tableOutput("tbl_summary_inner"),
        br(),
        div(class = "alert alert-info", p(interp))
      )
    )
  })

  # Inner table rendered separately so renderUI can reference it
  output$tbl_summary_inner <- renderTable({
    req(welfare_rv())
    df <- welfare_rv()
    optima <- df |>
      group_by(rule) |>
      filter(welfare == max(welfare)) |>
      slice(1) |>
      ungroup()

    w_ord  <- round(filter(optima, rule == "ordinal")$w,           3)
    w_car  <- round(filter(optima, rule == "cardinal")$w,          3)
    w_ref  <- round(filter(optima, rule == "cardinal_ref")$w,      3)
    eu_ord <- round(filter(optima, rule == "ordinal")$welfare,     4)
    eu_car <- round(filter(optima, rule == "cardinal")$welfare,    4)
    eu_ref <- round(filter(optima, rule == "cardinal_ref")$welfare, 4)
    best_eu <- max(eu_ord, eu_car, eu_ref)
    gap_ord <- round(best_eu - eu_ord, 4)
    gap_car <- round(best_eu - eu_car, 4)
    gap_ref <- round(best_eu - eu_ref, 4)

    data.frame(
      Rule          = c("Ordinal (NJ)",
                         "Cardinal (MI)",
                         sprintf("Cardinal ref-price (p_ref = %.2f)", input$p_ref)),
      `Optimal w*`  = c(w_ord,  w_car,  w_ref),
      `Max E[U]`    = c(eu_ord, eu_car, eu_ref),
      `Gap to best` = c(gap_ord, gap_car, gap_ref),
      check.names   = FALSE
    )
  }, na = "\u2014", striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

}

shinyApp(ui, server)
