insects <- read.table(
  file = "https://stat-methods.com/wp-content/uploads/2019/04/InsectSpraysKW.csv",
  header = TRUE, sep = ",")

kruskal.test(
  bugs ~ spray,
  data = insects
)

# Post Hoc Method one-Dunn's Test
library(dunn.test)

dunn <- dunn.test::dunn.test(
  x = insects$bugs,
  g = insects$spray,
  method = "bonferroni",
  alpha = 0.1,
  kw = TRUE
)

## Kable Code for Dunn's Test
knitr::kable(
  data.frame(comparison = dunn$comparisons, pvalues = dunn$P.adjusted),
  digits = 4,
  caption = "Post Hoc Dunn's Test--[insert method here] Adjustment",
  col.names = c("Comparison", "Adj. p-Value"),
  align = 'lc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")

# Post Hoc Method Two--DSCF Test
library(PMCMRplus)
dscf <- PMCMRplus::dscfAllPairsTest(
  bugs ~ spray,
  data = insects,
  na.action = "na.omit"
)

## Kable Code for DSCF
knitr::kable(
  dscf$p.value,
  digits = 3,
  caption = paste("Post Hoc-Dwass-Steel-Critchlow-Fligner Tests"),
  align = rep('c', nrow(dscf$p.value))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position") %>%
  kableExtra::footnote(general = "Rows and Columns are Treatment Levels.")

# Effect Sizes
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

knitr::kable(
  kw.PostHoc(insects$bugs, insects$spray),
  digits = 3,
  caption = "Post Hoc Comparison Effect Sizes",
  col.names = c("Pairwise Comparison","Hodges Lehmann Estimate",
                "Prob. Superiority"),
  align = 'lcc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")