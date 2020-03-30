ratLiver <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/ratLiver.dat",
  header = TRUE, sep = ",")
ratLiver$diet <- as.factor(ratLiver$diet)
options("contrasts" = c("contr.sum","contr.poly"))
ratModelA <- aov(liverWeight ~ diet, data = ratLiver)

# Pairwise Tests
## Tukey HSD
post.hoc <- TukeyHSD(ratModelA,conf.level=0.9)

## Kable Code for Tukey HSD
knitr::kable(
  post.hoc$diet,
  digits = 3,
  caption = "Post Hoc Tukey HSD Comparisons",
  col.names = c("Difference", "Lower Bound",
                "Upper Bound", "Adj. p-Value"),
  align = 'lcccc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")

## Pairwise Method
pairwiseList <- pairwise.t.test(ratLiver$liverWeight, ratLiver$diet,
                p.adjust.method = "bonferroni")

## Kable Code for Pairwise.t.Test
knitr::kable(
  pairwiseList$p.value,
  digits = 3,
  caption = paste("Post Hoc",
                  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                       pairwiseList$p.adjust.method,
                       perl = TRUE),
                  "Comparisons"),
  align = rep('c', nrow(pairwiseList$p.value))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position") %>%
  kableExtra::footnote(general = "Rows and Columns are Treatment Levels.")


## DescTools Pairwise Method
dtPHT <- DescTools::PostHocTest(aov(liverWeight~diet, data=ratLiver),
                       method = "bonf", conf.level = 0.9)

## Kable Code for DescTools
knitr::kable(
  dtPHT$diet,
  digits = 3,
  caption = paste("Post Hoc",
                  attr(dtPHT, "method"),
                  "Comparisons"),
  col.names = c("Difference", "Lower Bound",
                "Upper Bound", "Adj. p-Value"),
  align = 'lcccc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")

# Connecting Letters Report
multcompView::multcompLetters4(ratModelA, post.hoc,
                               threshold = 0.1)

## Boxplot with connecting letters--Does not allow you to set
## the threshold.
multcompView::multcompBoxplot(liverWeight ~ diet,
        data = ratLiver,
        compFn = "TukeyHSD",
        plotList = list(
        boxplot = list(fig = c(0, 0.85, 0, 1)),
        multcompLetters = list(fig = c(0.6, 1, 0.17, 0.87),
            fontsize = 12, fontface = NULL))
)

# Special Comparisons-Dunnett's Test
dunnett <- DescTools::DunnettTest(
  liverWeight ~ diet,
  data = ratLiver,
  control = "1",
  conf.level = 0.9)

## Kable Code for Dunnett's Test
knitr::kable(
  dunnett$`1`,
  digits = 3,
  caption = paste("Post Hoc Comparisons--Dunnett's Test"),
  col.names = c("Difference", "Lower Bound",
                "Upper Bound", "Adj. p-Value"),
  align = 'lcccc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")


# Effect Sizes
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

knitr::kable(
  anova.PostHoc(ratModelA),
  digits = 3,
  caption = "Post Hoc Comparison Effect Sizes",
  col.names = c("Pairwise Comparison","Cohen's d", "Hedge's g",
                "Prob. Superiority"),
  align = 'lccc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")
