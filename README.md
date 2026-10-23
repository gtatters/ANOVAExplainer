# ANOVA in a Nutshell: Signal versus Noise

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

A one-way ANOVA asks whether the differences among group means are bigger
than chance alone would produce. This app makes that idea visible. It
compares the spread **among** group means (the group mean square,
MS<sub>groups</sub>) with the spread of individuals **within** their groups
(the error mean square, MS<sub>error</sub>), and shows why their ratio, F,
sits near 1 when there is no real difference between groups.

The example data are hypothetical: simulated lizard body temperatures (°C)
measured in three habitats (A, B and C). They are not drawn from a
published study.

## The tabs

- **Overview** — the two kinds of spread, what the F ratio is, and how to
  use the app
- **1. Signal vs. noise** — a single dataset with each lizard, its group
  mean and the grand mean. Teal lines show within-group deviations (noise)
  and orange bars show between-group deviations (signal). A bar plot
  compares MS<sub>groups</sub> with MS<sub>error</sub>, and a summary panel
  reports MS<sub>groups</sub>, MS<sub>error</sub>, F and p. An optional R
  ANOVA table (`summary(aov())`) can be shown for comparison
- **2. Null vs. alternative** — the whole study is repeated 1000 times
  under either Scenario 1 (one shared population, so the null is true) or
  Scenario 2 (three genuinely different populations). A histogram of the
  1000 F values is drawn over the theoretical F distribution, with the
  critical value marked. Under the null, about 5% of studies are false
  positives; under the alternative, the percentage that are significant is
  the study's power

## How to use

In each tab:

- Read the short description on the left.
- Adjust the sliders for the true difference among habitat means, the
  within-group spread (σ) and the number of lizards per habitat (n).
- Press **Resample** to draw a new random dataset with the same settings.
- Read the panel below the plots. The **bold** values are the ones that
  change as you experiment.

Useful things to try:

- Set the difference to 0 in Tab 1 and press **Resample** several times to
  see how much F wobbles by chance alone.
- Increase n and watch MS<sub>groups</sub> grow for the same true
  difference; the signal is multiplied by sample size.
- In Tab 2, switch between the two scenarios and watch the F histogram
  shift away from the null curve.

## Learning goals

- Understand ANOVA as a comparison of two variance estimates, not as a
  comparison of means one pair at a time
- Connect MS<sub>groups</sub> and MS<sub>error</sub> to what they look like
  in a plot of the data
- See why F is close to 1 when the null hypothesis is true, and what the
  critical value and p-value mean in terms of repeated studies
- See how sample size, within-group variability and the size of the true
  difference together determine power

## Setup notes

- No packages beyond `shiny` are required. All plots use base R graphics.

  ```r
  shinylive::export(".", "docs")
  ```

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only — no tidyverse, ggplot, car,
or other package dependencies).

Glenn Tattersall, PhD
