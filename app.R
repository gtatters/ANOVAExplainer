# app.R
# ANOVA in a Nutshell: Signal versus Noise
#
# Shows how a one-way ANOVA compares the spread AMONG group means
# (between-group variance) with the spread WITHIN groups (within-group
# variance), and why their ratio (F) is near 1 when the null is true.
#
# Example data are hypothetical (simulated lizard body temperatures in
# three habitats); they are not drawn from a published study.
#
# Base R only - no tidyverse / ggplot / MASS / car dependencies.
# Written for older R (no native pipe, no \(x) lambdas).

# ---- shared palette ---------------------------------------------------------
blue   <- "#195190"
teal   <- "#009499"
orange <- "#E07B39"
red    <- "#C0392B"
grey   <- "gray60"

grp_lab <- c("A", "B", "C")
base_mu <- 30          # baseline mean body temperature (deg C)
n_max   <- 30          # largest sample size offered by the sliders
n_sims  <- 1000        # simulated experiments in Tab 2

# ---- small helpers ----------------------------------------------------------
fmt_p <- function(p) if (p < 0.001) "< 0.001" else formatC(p, digits = 3, format = "f")

# big number; optional small grey note on the same line (used for F degrees of freedom)
big_num <- function(txt, col = "#222", note = NULL) {
  tags$p(style = paste0("font-size:28px;font-weight:700;color:", col, ";margin:0;"), txt,
         if (!is.null(note))
           tags$span(style = "font-size:13px;font-weight:400;color:#777;", note))
}

df_note <- function(df1, df2) paste0("df = ", df1, ", ", df2)

f_lab <- function(df1, df2) paste0("F(", df1, ", ", df2, ")")

stat_label <- function(txt) {
  p(txt, style = "color:#555;font-size:13px;margin-bottom:2px;")
}

# One-way ANOVA pieces for a list of numeric vectors
anova_parts <- function(ylist) {
  k   <- length(ylist)
  n_i <- sapply(ylist, length)
  N   <- sum(n_i)
  gm  <- sapply(ylist, mean)
  G   <- sum(n_i * gm) / N
  ssb <- sum(n_i * (gm - G)^2)
  ssw <- sum(sapply(seq_len(k), function(i) sum((ylist[[i]] - gm[i])^2)))
  df1 <- k - 1; df2 <- N - k
  msb <- ssb / df1; msw <- ssw / df2
  Fv  <- msb / msw
  list(gm = gm, G = G, ssb = ssb, ssw = ssw, df1 = df1, df2 = df2,
       msb = msb, msw = msw, F = Fv,
       p = pf(Fv, df1, df2, lower.tail = FALSE))
}

# ================================ UI =========================================
ui <- fluidPage(
  titlePanel("ANOVA in a Nutshell: Signal versus Noise",
             windowTitle = "ANOVA in a Nutshell"),
  
  tags$head(tags$style(HTML(
    ".action-button { color:#fff; background-color:#569BBD; border:none; }
     .action-button:hover { color:#fff; background-color:#3E7C99; }
     .action-button:active { transform:scale(0.97); }

     .nav-tabs > li.active > a,
     .nav-tabs > li.active > a:hover,
     .nav-tabs > li.active > a:focus {
       color: #009499 !important;
       font-size: 12px; font-weight: bold;
       background-color: #f0fafa !important;
     }
     .nav-tabs > li > a {
       color: #666;
       font-size: 12px;
       padding: 5px 9px;
     }
     .nav-tabs > li > a:hover {
       color: #009499;
       background-color: #f0fafa;
     }
     pre { font-size: 12px; }"
  ))),
  
  tabsetPanel(
    type = "tabs",
    
    # ======= OVERVIEW =======================================================
    tabPanel(
      "Overview",
      fluidRow(
        column(
          width = 8,
          h2("ANOVA in a nutshell",
             style = paste0("color:", teal, "; font-weight:700;")),
          p(style = "font-size:17px; line-height:1.6;",
            "Analysis of variance (ANOVA) asks a simple question: are the differences ",
            "between group averages bigger than we would expect from chance alone? ",
            "Even when all groups come from the same population, their sample means ",
            "will never be identical. ANOVA works out how much the means ", em("should"),
            " wobble by chance, and compares that with how much they actually differ."),
          h4(style = paste0("color:", teal, ";"), "Two kinds of spread"),
          tags$ol(
            style = "font-size:16px; line-height:1.9;",
            tags$li(strong("Within-group variance (noise):"),
                    " how far individuals sit from their own group's mean. ",
                    "This is the ", strong("error mean square, MS", tags$sub("error")), "."),
            tags$li(strong("Between-group variance (signal):"),
                    " how far the group means sit from the overall (grand) mean. ",
                    "This is the ", strong("group mean square, MS", tags$sub("groups")), ".")
          ),
          p(style = "font-size:16px; line-height:1.6;",
            "The F ratio is simply ", strong("between \u00f7 within"), ", or ",
            strong("MS", tags$sub("groups"), " \u00f7 MS", tags$sub("error")), ". If the groups ",
            "really come from one population, both numbers estimate the same thing and ",
            "F hovers around 1. If the populations truly differ, the between-group ",
            "variance is inflated and F grows."),
          h4(style = paste0("color:", teal, ";"), "The tabs"),
          tags$ol(
            style = "font-size:16px; line-height:1.9;",
            tags$li(strong("Signal vs. noise:"),
                    " see the within- and between-group deviations drawn on one dataset."),
            tags$li(strong("Null vs. alternative:"),
                    " repeat the experiment 1000 times to see what F looks like when ",
                    "there is, or is not, a real difference.")
          ),
          br(),
          p(style = "font-size:15px; color:#555;",
            em("The example is hypothetical: lizard body temperatures (\u00b0C) measured ",
               "in three habitats. All data are simulated."))
        ),
        column(
          width = 4,
          br(),
          wellPanel(
            p(strong("A useful way to think about it")),
            p("Imagine three thermometers placed in three habitats. Each one flickers ",
              "a little from reading to reading (within-group noise). If the habitat ",
              "averages differ by much more than that flicker, the habitats probably ",
              "really differ. If they differ by about as much as the flicker, you ",
              "cannot tell them apart."),
            hr(),
            helpText("Glenn Tattersall, PhD"),
            helpText("For use in BIOL 3P96 - Biostatistics")
          )
        )
      )
    ),
    
    # ======= 1. SIGNAL VS NOISE =============================================
    tabPanel(
      "1. Signal vs. Noise",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The idea")),
            p("Each dot is one lizard. The black bar in each group is that group's mean, ",
              "and the dashed grey line is the grand mean of all lizards together."),
            p(span(style = paste0("color:", teal, ";font-weight:bold;"), "Teal lines"),
              " show how far each lizard is from its own group mean: the ",
              strong("within-group"), " spread, or noise."),
            p(span(style = paste0("color:", orange, ";font-weight:bold;"), "Orange bars"),
              " show how far each group mean is from the grand mean: the ",
              strong("between-group"), " spread, or signal."),
            hr(),
            sliderInput("t1_eff", "Difference among true habitat means (\u00b0C):",
                        value = 1.0, min = -3, max = 3, step = 0.1),
            sliderInput("t1_sd", "Within-group spread, \u03c3 (\u00b0C):",
                        value = 1.5, min = 0.3, max = 3, step = 0.1),
            sliderInput("t1_n", "Lizards per habitat (n):",
                        value = 10, min = 3, max = n_max, step = 1),
            checkboxInput("t1_show_w", "Show within-group deviations", TRUE),
            checkboxInput("t1_show_b", "Show between-group deviations", TRUE),
            checkboxInput("t1_show_r", "Show R ANOVA table", FALSE),
            actionButton("t1_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(7, plotOutput("t1_dots", height = "340px")),
            column(5, plotOutput("t1_bars", height = "340px"))
          ),
          wellPanel(
            fluidRow(
              column(3, stat_label(HTML("MS<sub>groups</sub> (between groups, signal)")),
                     uiOutput("t1_msb")),
              column(3, stat_label(HTML("MS<sub>error</sub> (within groups, noise)")),
                     uiOutput("t1_msw")),
              column(3, stat_label(HTML("F = MS<sub>groups</sub> \u00f7 MS<sub>error</sub>")),
                     uiOutput("t1_F")),
              column(3, stat_label("p-value"),                     uiOutput("t1_p"))
            )
          ),
          wellPanel(div(uiOutput("t1_verdict"), align = "justify")),
          conditionalPanel(
            condition = "input.t1_show_r == true",
            verbatimTextOutput("t1_rcode")
          )
        )
      )
    ),
    
    # ======= 2. NULL VS ALTERNATIVE =========================================
    tabPanel(
      "2. Null vs. Alternative",
      br(),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            p(strong("The idea")),
            p(strong("Scenario 1 (null):"), " all three habitats share one population. ",
              "Any differences among the sample means are due to chance alone."),
            p(strong("Scenario 2 (alternative):"), " the habitats are truly different ",
              "populations with different average temperatures."),
            p("Here we repeat the whole study ", n_sims, " times and record F each time. ",
              "Under Scenario 1, F piles up near 1 and only about 5% of studies cross the ",
              "red line by bad luck. Under Scenario 2, F shifts to the right."),
            hr(),
            radioButtons("t2_scen", "True state of the world:",
                         choices = c("Scenario 1: one population (no real difference)" = "null",
                                     "Scenario 2: three different populations"         = "alt"),
                         selected = "null"),
            conditionalPanel(
              condition = "input.t2_scen == 'alt'",
              sliderInput("t2_eff", "Difference among true habitat means (\u00b0C):",
                          value = 1.0, min = 0.1, max = 3, step = 0.1)
            ),
            sliderInput("t2_sd", "Within-group spread, \u03c3 (\u00b0C):",
                        value = 1.5, min = 0.3, max = 3, step = 0.1),
            sliderInput("t2_n", "Lizards per habitat (n):",
                        value = 10, min = 3, max = n_max, step = 1),
            actionButton("t2_new", "Resample")
          )
        ),
        column(
          width = 8,
          fluidRow(
            column(6, plotOutput("t2_pop",  height = "380px")),
            column(6, plotOutput("t2_hist", height = "380px"))
          ),
          wellPanel(
            fluidRow(
              column(3, stat_label("Average F"),          uiOutput("t2_medF")),
              column(3, stat_label("Critical F (\u03b1 = 0.05)"),  uiOutput("t2_crit")),
              column(3, stat_label("Studies with p < 0.05"),       uiOutput("t2_pct")),
              column(3, stat_label("Average F if no real difference"), uiOutput("t2_nullF"))
            )
          ),
          wellPanel(div(uiOutput("t2_verdict"), align = "justify"))
        )
      )
    )
    
  )   # end tabsetPanel
)   # end fluidPage


# ============================== SERVER =======================================
server <- function(input, output, session) {
  
  # ===== 1. SIGNAL VS NOISE =================================================
  # Random draws depend only on the Resample button, so sliders move the
  # same lizards smoothly rather than redrawing them.
  t1_raw <- reactive({
    set.seed(2100 + input$t1_new)
    list(z = matrix(rnorm(3 * n_max), nrow = 3),
         j = matrix(runif(3 * n_max, -0.5, 0.5), nrow = 3))
  })
  
  t1_res <- reactive({
    r   <- t1_raw()
    n   <- input$t1_n
    mu  <- base_mu + c(-1, 0, 1) * input$t1_eff
    yl  <- lapply(1:3, function(g) mu[g] + input$t1_sd * r$z[g, 1:n])
    a   <- anova_parts(yl)
    a$y <- yl; a$jit <- r$j[, 1:n, drop = FALSE]; a$mu <- mu; a$n <- n
    a
  })
  
  output$t1_dots <- renderPlot({
    a  <- t1_res()
    gx <- 1:3
    lim_half <- max(6, 3 + 3.2 * input$t1_sd)   # moves with sliders, not with Resample
    par(mar = c(2.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(NULL, xlim = c(0.4, 3.75), ylim = base_mu + c(-1, 1) * lim_half,
         xaxt = "n", xlab = "", ylab = "Body temperature (\u00b0C)",
         main = "Each lizard, its group mean, and the grand mean", bty = "l")
    axis(1, at = gx, labels = paste("Habitat", grp_lab))
    abline(h = a$G, col = "gray45", lty = 2, lwd = 1.8)
    for (g in 1:3) {
      xs <- gx[g] + a$jit[g, ] * 0.55
      if (input$t1_show_w)
        segments(xs, a$y[[g]], xs, a$gm[g], col = adjustcolor(teal, 0.75), lwd = 1.4)
      points(xs, a$y[[g]], pch = 19, col = adjustcolor("gray30", 0.70), cex = 1.0)
      segments(gx[g] - 0.36, a$gm[g], gx[g] + 0.36, a$gm[g], col = "black", lwd = 3.5)
      if (input$t1_show_b)
        segments(gx[g] + 0.45, a$gm[g], gx[g] + 0.45, a$G, col = orange, lwd = 7, lend = 1)
    }
    legend("topleft", bty = "n", cex = 0.9,
           lty = c(1, 1, 1, 2), lwd = c(1.4, 6, 3.5, 1.8),
           col = c(teal, orange, "black", "gray45"),
           legend = c("Within: lizard to its group mean",
                      "Between: group mean to grand mean",
                      "Group mean", "Grand mean"))
  })
  
  output$t1_bars <- renderPlot({
    a    <- t1_res()
    vals <- c(a$msb, a$msw)
    ytop <- max(vals) * 1.25
    par(mar = c(3.2, 4.5, 4, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    bp <- barplot(vals, col = c(orange, teal), border = "white",
                  names.arg = c("", ""),
                  ylim = c(0, ytop), ylab = "Variance estimate (mean square)")
    title(main = "Signal compared with noise", line = 2.2)
    mtext(expression(MS[groups], MS[error]), side = 1, at = bp, line = 0.7, cex = 1.15)
    mtext(c("between (signal)", "within (noise)"), side = 1, at = bp, line = 1.9,
          cex = 0.95, col = "gray30")
    abline(h = a$msw, col = "gray45", lty = 2, lwd = 1.8)
    text(mean(bp), a$msw, "noise level", pos = 3, cex = 0.9, col = "gray35")
    text(bp, vals + ytop * 0.04, labels = formatC(vals, digits = 2, format = "f"),
         cex = 1.2, font = 2)
    mtext(paste0(f_lab(a$df1, a$df2), " = ", formatC(a$F, digits = 2, format = "f"),
                 "  (the orange bar is ", formatC(a$F, digits = 1, format = "f"),
                 "\u00d7 the teal bar)"),
          side = 3, line = 0.6, cex = 0.95, col = "gray25")
  })
  
  output$t1_msb <- renderUI(big_num(formatC(t1_res()$msb, digits = 2, format = "f"), orange))
  output$t1_msw <- renderUI(big_num(formatC(t1_res()$msw, digits = 2, format = "f"), teal))
  output$t1_F   <- renderUI({
    a <- t1_res()
    big_num(formatC(a$F, digits = 2, format = "f"), blue, df_note(a$df1, a$df2))
  })
  output$t1_p   <- renderUI({
    p <- t1_res()$p
    big_num(fmt_p(p), if (p < 0.05) red else "#222")
  })
  
  output$t1_verdict <- renderUI({
    a <- t1_res(); n <- a$n; sd <- input$t1_sd
    expect_b <- sd^2 + n * input$t1_eff^2      # variance of true means (-d, 0, d) is d^2
    sig_txt <- if (a$p < 0.05)
      paste0("this is <b>more than we would expect from chance</b> (p ",
             if (a$p < 0.001) "< 0.001" else paste0("= ", fmt_p(a$p)),
             "), so we conclude the habitats differ.")
    else
      paste0("this is <b>not more than chance could easily produce</b> (p = ",
             fmt_p(a$p), "), so we cannot conclude the habitats differ.")
    null_txt <- if (input$t1_eff == 0)
      paste0("Right now the true habitat means are identical, so both bars are estimating ",
             "the same thing (\u03c3\u00b2 = ", formatC(sd^2, digits = 2, format = "f"),
             ") and F should be close to 1. Press <b>Resample</b> a few times to see how ",
             "much F wobbles by chance alone. ")
    else
      paste0("With a true difference of <b>", abs(input$t1_eff), " \u00b0C</b> between ",
             "neighbouring habitats (Habitat ", if (input$t1_eff > 0) "C" else "A",
             " is the warmest), the between-group variance (MS<sub>groups</sub>) is expected to be about <b>",
             formatC(expect_b, digits = 1, format = "f"), "</b>, compared with about <b>",
             formatC(sd^2, digits = 2, format = "f"), "</b> for the within-group variance (MS<sub>error</sub>). ",
             "Notice that the signal is multiplied by n: more lizards per habitat makes a ",
             "real difference easier to see. ")
    HTML(paste0(
      "The group means are spread out with a between-group variance (MS<sub>groups</sub>) of <b>",
      formatC(a$msb, digits = 2, format = "f"), "</b>, while individual lizards vary ",
      "around their own group mean with a within-group variance (MS<sub>error</sub>) of <b>",
      formatC(a$msw, digits = 2, format = "f"), "</b>. The ratio ", f_lab(a$df1, a$df2),
      " = <b>", formatC(a$F, digits = 2, format = "f"), "</b>, and ", sig_txt, " ", null_txt
    ))
  })
  
  output$t1_rcode <- renderText({
    a  <- t1_res()
    df <- data.frame(temp    = round(unlist(a$y), 2),
                     habitat = factor(rep(grp_lab, each = a$n)))
    out <- capture.output(summary(aov(temp ~ habitat, data = df)))
    paste(c("> summary(aov(temp ~ habitat, data = lizards))", out), collapse = "\n")
  })
  
  # ===== 2. NULL VS ALTERNATIVE =============================================
  t2_raw <- reactive({
    set.seed(2200 + input$t2_new)
    array(rnorm(n_sims * 3 * n_max), dim = c(n_sims, 3, n_max))
  })
  
  t2_res <- reactive({
    z   <- t2_raw()
    n   <- input$t2_n
    sd  <- input$t2_sd
    eff <- if (input$t2_scen == "alt") input$t2_eff else 0
    mu  <- base_mu + c(-1, 0, 1) * eff
    zz  <- z[, , 1:n, drop = FALSE]
    # group means and within-group sums of squares for every simulated study
    zm  <- apply(zz, c(1, 2), mean)                        # n_sims x 3
    ssw <- sd^2 * rowSums(apply(zz, c(1, 2), function(v) sum((v - mean(v))^2)))
    gm  <- sweep(sd * zm, 2, mu, "+")
    G   <- rowMeans(gm)
    ssb <- n * rowSums((gm - G)^2)
    df1 <- 2; df2 <- 3 * n - 3
    Fv  <- (ssb / df1) / (ssw / df2)
    list(F = Fv, df1 = df1, df2 = df2, crit = qf(0.95, df1, df2),
         mu = mu, sd = sd, n = n, eff = eff, gm1 = gm[1, ])
  })
  
  output$t2_pop <- renderPlot({
    r  <- t2_res()
    xs <- seq(base_mu - 12, base_mu + 12, length.out = 400)   # fixed axis
    cols <- c(teal, blue, orange)
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    ymax <- dnorm(0, 0, r$sd) * 1.35
    plot(NULL, xlim = range(xs), ylim = c(0, ymax),
         xlab = "Body temperature (\u00b0C)", ylab = "Density",
         main = if (r$eff == 0) "Scenario 1: one shared population"
         else "Scenario 2: three different populations",
         bty = "l", yaxt = "n")
    if (r$eff == 0) {
      dy <- dnorm(xs, base_mu, r$sd)
      polygon(c(xs, rev(xs)), c(dy, rep(0, length(xs))),
              col = adjustcolor("gray60", 0.35), border = "gray40", lwd = 2)
      text(base_mu, max(dy) * 1.08, "A, B and C", cex = 1.1, font = 2, col = "gray30")
    } else {
      for (g in 1:3) {
        dy <- dnorm(xs, r$mu[g], r$sd)
        polygon(c(xs, rev(xs)), c(dy, rep(0, length(xs))),
                col = adjustcolor(cols[g], 0.20), border = cols[g], lwd = 2)
        text(r$mu[g], max(dy) * 1.08, grp_lab[g], cex = 1.2, font = 2, col = cols[g])
      }
    }
    # sample means from the first simulated study, as triangles on the axis
    points(r$gm1, rep(0, 3), pch = 17, cex = 1.8,
           col = if (r$eff == 0) "gray25" else cols)
    text(r$gm1, rep(ymax * 0.07, 3), grp_lab, cex = 0.9, font = 2,
         col = if (r$eff == 0) "gray25" else cols)
    legend("topright", bty = "n", cex = 0.9, pch = 17, col = "gray25",
           legend = "Sample means (one study)")
  })
  
  output$t2_hist <- renderPlot({
    r    <- t2_res()
    # x-axis grows with the data: covers 99% of F values, rounded up to a multiple of 5
    xmax <- max(10, 5 * ceiling(quantile(r$F, 0.99) / 5))
    Fc   <- pmin(r$F, xmax)                        # pile the most extreme 1% in last bin
    # bar width set by where the F values actually are (about 25 bars across them)
    q    <- quantile(r$F, c(0.01, 0.99))
    bw   <- max((q[2] - q[1]) / 25, xmax / 300)
    brks <- seq(0, ceiling(xmax / bw) * bw, by = bw)
    h    <- hist(Fc, breaks = brks, plot = FALSE)
    # y-axis follows the bars; the blue curve may run off the top when F is large
    dens_max <- max(h$density)
    bar_col  <- ifelse(h$mids >= r$crit, adjustcolor(orange, 0.85), adjustcolor(teal, 0.70))
    par(mar = c(4.5, 4.5, 3, 1), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    plot(h, freq = FALSE, col = bar_col, border = "white",
         xlim = c(0, xmax), ylim = c(0, dens_max * 1.15),
         xlab = "F ratio", ylab = "Density",
         main = paste0(f_lab(r$df1, r$df2), " from ", n_sims, " repeated studies"))
    box(bty = "l")
    xs <- seq(0.02, xmax, 0.02)
    lines(xs, df(xs, r$df1, r$df2), col = blue, lwd = 2.5)
    abline(v = r$crit, col = red, lwd = 2, lty = 2)
    mtext(formatC(r$crit, digits = 2, format = "f"), side = 3, at = r$crit,
          line = 0.1, cex = 0.9, col = red, font = 2)
    abline(v = 1, col = "gray45", lwd = 1.5, lty = 3)
    leg_pos <- if (median(r$F) > xmax / 2) "topleft" else "topright"   # keep clear of bars
    legend(leg_pos, bty = "n", cex = 0.85, inset = c(0.04, 0),
           lty = c(1, 2, 3, NA, NA), lwd = c(2.5, 2, 1.5, NA, NA),
           pch = c(NA, NA, NA, 15, 15), pt.cex = 1.8,
           col = c(blue, red, "gray45", adjustcolor(teal, 0.70), adjustcolor(orange, 0.85)),
           legend = c(paste0("Expected ", f_lab(r$df1, r$df2), " if Scenario 1 is true"),
                      "Critical F (\u03b1 = 0.05)", "F = 1",
                      "Not significant", "Significant (p < 0.05)"))
    if (any(r$F > xmax))
      mtext(paste0("F values above ", xmax, " are shown in the last bar"),
            side = 1, line = 3.6, adj = 1, cex = 0.8, col = "gray50")
  })
  
  output$t2_medF <- renderUI({
    r <- t2_res()
    big_num(formatC(mean(r$F), digits = 2, format = "f"), blue, df_note(r$df1, r$df2))
  })
  output$t2_crit <- renderUI({
    r <- t2_res()
    big_num(formatC(r$crit, digits = 2, format = "f"), red, df_note(r$df1, r$df2))
  })
  output$t2_pct  <- renderUI({
    r <- t2_res(); pct <- 100 * mean(r$F >= r$crit)
    big_num(paste0(formatC(pct, digits = 1, format = "f"), "%"), orange)
  })
  output$t2_nullF <- renderUI({
    r <- t2_res()                               # mean of an F distribution = df2 / (df2 - 2)
    big_num(formatC(r$df2 / (r$df2 - 2), digits = 2, format = "f"), "gray45", df_note(r$df1, r$df2))
  })
  
  output$t2_verdict <- renderUI({
    r   <- t2_res()
    pct <- formatC(100 * mean(r$F >= r$crit), digits = 1, format = "f")
    med <- formatC(mean(r$F), digits = 2, format = "f")
    crit <- paste0(f_lab(r$df1, r$df2), " = ", formatC(r$crit, digits = 2, format = "f"))
    df_txt <- paste0(
      " With ", r$n, " lizards per habitat the degrees of freedom are ", r$df1,
      " for groups (3 habitats \u2212 1) and ", r$df2, " for error (", 3 * r$n,
      " lizards \u2212 3 habitats). Larger samples give a slightly lower critical value, ",
      "but the change is small once there are more than about 20 error degrees of freedom.")
    if (r$eff == 0) {
      HTML(paste0(
        "All three habitats come from the same population, yet the sample means still ",
        "differ from study to study. The average F is <b>", med, "</b>, close to 1, because ",
        "MS<sub>groups</sub> and MS<sub>error</sub> are estimating the same thing. ",
        "Only <b>", pct, "%</b> of studies exceed the critical value of <b>", crit,
        "</b>. These are ", strong("false positives"), ": studies that would wrongly ",
        "conclude the habitats differ. By design this happens about 5% of the time. ",
        "Notice that the teal bars follow the blue curve: that curve is the F distribution, ",
        "and it is what the p-value is calculated from.", df_txt
      ))
    } else {
      HTML(paste0(
        "The habitats really do differ, by <b>", r$eff, " \u00b0C</b> between neighbours. ",
        "The real difference adds extra spread among the group means, so F shifts to the ",
        "right of the blue curve (average F = <b>", med, "</b>). <b>", pct,
        "%</b> of studies cross the critical value of <b>", crit, "</b> and correctly ",
        "detect the difference; this percentage is the study's ", strong("power"), ". ",
        "The remaining studies miss a real effect. Increase n or reduce \u03c3 and watch ",
        "power rise; shrink the difference and watch the two distributions merge.", df_txt
      ))
    }
  })
  
}  # end server

# ---- launch -----------------------------------------------------------------
shinyApp(ui = ui, server = server)