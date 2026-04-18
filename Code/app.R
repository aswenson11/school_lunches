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
          "Equilibrium bid functions β*(c) for both scoring rules at the current w.",
          " Dashed line = bid at cost (zero markup).",
          " Points above the dashed line represent the markup each firm extracts in equilibrium."
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

  # ── BNE at current w_disp — both rules (for bid function plot) ──────────────
  #
  # n_grid = 35, n_search = 120: ~3–6s per rule. bindCache prevents
  # recomputation when only lambda or run_welfare changes.
  bne_both <- reactive({
    req(
      input$c1_range[1] < input$c1_range[2],
      input$c2_range[1] < input$c2_range[2]
    )
    withProgress(message = "Solving BNE…", value = 0.3, {
      ord <- solve_bne(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$w_disp, input$S, rule = "ordinal",
        n_grid = 35, n_search = 120, max_iter = 50
      )
      setProgress(0.65)
      car <- solve_bne(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$w_disp, input$S, rule = "cardinal",
        n_grid = 35, n_search = 120, max_iter = 50
      )
      setProgress(1.0)
      list(ordinal = ord, cardinal = car)
    })
  }) |> bindCache(
    input$q1, input$q2,
    input$c1_range, input$c2_range,
    input$w_disp, input$S
  )

  # ── Welfare curves (button-triggered, slower) ────────────────────────────────
  welfare_rv <- eventReactive(input$run_welfare, {
    req(
      input$c1_range[1] < input$c1_range[2],
      input$c2_range[1] < input$c2_range[2]
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
      setProgress(0.55, detail = "Cardinal rule (MI)…")
      car <- welfare_over_w(
        input$q1, input$q2,
        input$c1_range[1], input$c1_range[2],
        input$c2_range[1], input$c2_range[2],
        input$lambda, input$S, rule = "cardinal",
        n_w = 13, n_grid = 28, n_search = 90
      )
      setProgress(1.0)
      rbind(ord, car)
    })
  })

  # ── Plot: bid functions ──────────────────────────────────────────────────────
  output$plot_bids <- renderPlot({
    bne <- bne_both()

    df <- rbind(
      data.frame(firm = "Firm 1", rule = "Ordinal (NJ)",
                 cost = bne$ordinal$c1,  bid = bne$ordinal$b1),
      data.frame(firm = "Firm 1", rule = "Cardinal (MI)",
                 cost = bne$cardinal$c1, bid = bne$cardinal$b1),
      data.frame(firm = "Firm 2", rule = "Ordinal (NJ)",
                 cost = bne$ordinal$c2,  bid = bne$ordinal$b2),
      data.frame(firm = "Firm 2", rule = "Cardinal (MI)",
                 cost = bne$cardinal$c2, bid = bne$cardinal$b2)
    )

    ggplot(df, aes(x = cost, y = bid, color = rule, linetype = firm)) +
      geom_abline(slope = 1, intercept = 0,
                  color = "grey65", linetype = "dashed", linewidth = 0.7) +
      geom_line(linewidth = 1.3) +
      scale_color_manual(
        values = c("Ordinal (NJ)"  = "#8C1515",
                   "Cardinal (MI)" = "#0098DB")
      ) +
      scale_linetype_manual(
        values = c("Firm 1" = "solid", "Firm 2" = "longdash")
      ) +
      labs(
        title   = sprintf("Equilibrium bid functions    w = %.2f  |  q\u2081 = %.2f  q\u2082 = %.2f",
                           input$w_disp, input$q1, input$q2),
        x       = "True cost  c",
        y       = "Equilibrium bid  \u03b2*(c)",
        color   = "Scoring rule",
        linetype = "Firm"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 13))
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

    rule_labels <- c("ordinal"  = "Ordinal (NJ)",
                     "cardinal" = "Cardinal (MI)")
    rule_colors <- c("ordinal"  = "#8C1515",
                     "cardinal" = "#0098DB")

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

    w_ord  <- round(filter(optima, rule == "ordinal")$w,       3)
    w_car  <- round(filter(optima, rule == "cardinal")$w,      3)
    eu_ord <- round(filter(optima, rule == "ordinal")$welfare,  4)
    eu_car <- round(filter(optima, rule == "cardinal")$welfare, 4)
    loss   <- round(eu_car - eu_ord, 4)

    # Interpretation message
    w_dev  <- abs(w_ord - input$lambda)
    interp <- if (w_dev > 0.10) {
      sprintf(
        "Ordinal scoring forces the district to set w* = %.2f to compensate for
        rank-based distortion, a gap of %.2f from its true preference \u03bb = %.2f.
        Under cardinal scoring the district can set w* = %.2f, which
        is much closer to \u03bb. The welfare loss from being constrained to
        ordinal scoring is %.4f utility units.",
        w_ord, w_dev, input$lambda, w_car, loss
      )
    } else {
      sprintf(
        "Under these parameters the two rules produce similar optimal weights
        (ordinal w* = %.2f, cardinal w* = %.2f). Try widening the spread
        between the two firms' cost distributions, or increasing the quality gap,
        to amplify the distortion from ordinal scoring.",
        w_ord, w_car
      )
    }

    tbl <- data.frame(
      Rule            = c("Ordinal (NJ)", "Cardinal (MI)",
                           "Welfare loss  (cardinal \u2212 ordinal)"),
      `Optimal w*`    = c(w_ord, w_car, NA),
      `Max E[U]`      = c(eu_ord, eu_car, loss),
      check.names     = FALSE,
      stringsAsFactors = FALSE
    )

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

    w_ord  <- round(filter(optima, rule == "ordinal")$w,      3)
    w_car  <- round(filter(optima, rule == "cardinal")$w,     3)
    eu_ord <- round(filter(optima, rule == "ordinal")$welfare, 4)
    eu_car <- round(filter(optima, rule == "cardinal")$welfare, 4)
    loss   <- round(eu_car - eu_ord, 4)

    data.frame(
      Rule         = c("Ordinal (NJ)", "Cardinal (MI)",
                        "Welfare loss  (cardinal \u2212 ordinal)"),
      `Optimal w*` = c(w_ord, w_car, NA),
      `Max E[U]`   = c(eu_ord, eu_car, loss),
      check.names  = FALSE
    )
  }, na = "\u2014", striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

}

shinyApp(ui, server)
