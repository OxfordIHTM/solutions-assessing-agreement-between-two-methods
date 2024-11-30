# Workflow to perform Bland and Altman analysis --------------------------------

## Load functions created for the Bland and Altman analysis ----
source("functions.R")


## Read the example Bland and Altman dataset ----

ba_data <- read_ba_data(
  url = "https://raw.githubusercontent.com/OxfordIHTM/teaching_datasets/main/ba.dat",
  destfile = "data/ba.dat"
)


## Calculate Bland and Altman metrics from ba_data ----

ba_metrics <- calculate_ba_metrics(df = ba_data, m1 = "Wright", m2 = "Mini")


## Create a Bland and Altman plot ----

plot_ba(
  ba_metrics,
  title = "Wrigth vs Mini-Wright",
  xlab = "Mean PEFR (per subject)",
  ylab = "Difference in PEFR (per subject)",
  pch = 21, col = "darkblue", bg = "lightblue", cex = 1.2
)

