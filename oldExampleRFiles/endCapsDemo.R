# Packages
packages <- c("tidyverse", "hasseDiagram","knitr",
              "kableExtra", "car", "parameters",
              "DescTools", "ggplot2", "lattice",
              "psych")
lapply(packages, library, character.only = TRUE)

# Import the Data
path <- url(
  "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/onewayExplorations.Rdata"
)
load(path)
close(path)

# Data are in endCaps data frame
# Renaming design values to add clarity
endCaps$design <- dplyr::recode_factor(endCaps$design,
                                       `1` = "Design 1",
                                       `2` = "Design 2",
                                       `3` = "Design 3")

# Create Hasse Diagram
salesLabels <- c("1 Grand Mean 1", "3 Design 2", "15 (Store/Error) 12")
salesMat <- matrix(data = F, nrow = 3, ncol = 3)
salesMat[1, c(2:3)] = salesMat[2, 3] = T

hasseDiagram::hasse(salesMat, salesLabels)

# Explore Data

ggplot2::ggplot(data = endCaps,
                mapping = aes(x = sales)) +
  ggplot2::geom_histogram(col = "white", fill = "#001E44",
                          closed="left", boundry = 4,
                          binwidth = 0.5) +
  ggplot2::theme_bw() +
  xlab("Percent Increase in Soft Drink Sales")

lattice::densityplot(
  endCaps$sales,
  na.rm = TRUE,
  xlab = "Percent Increase in Soft Drink Sales",
  lwd = 4,
  pch = 20,
  col = "#001E44"
)

descSales <- psych::describeBy(
  x = endCaps$sales,
  group = endCaps$design,
  mat = TRUE,
  na.rm = TRUE,
  interp = FALSE,
  skew = TRUE,
  ranges = TRUE,
  IQR = FALSE,
  quant = c(0.25, 0.75))


# Drop columns and re-arrange
descSales <- subset(descSales, select = -c(vars, trimmed, range, se))

descSales <- descSales[, c("n", "min", "Q0.25", "median",
                           "Q0.75", "max", "mad",
                           "mean", "sd", "skew",
                           "kurtosis")]

names(descSales) <- c("n", "Min", "Q1", "Median", "Q3",
                      "Max", "MAD","SAM", "SASD",
                      "S.Skew", "SEKurt")

row.names(descSales) <- c("Design 1", "Design 2", "Design 3")

knitr::kable(descSales,
             caption = "Values of Descriptive Statistics for Percent Increase in Sales",
             align = rep('c', ncol(descSales)),
             format = "latex",
             digits = 2) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")%>%
  kableExtra::row_spec( row = 0, extra_latex_after = "\\arrayrulecolor{black}") %>%
  kableExtra::row_spec(row = 1, extra_latex_after = "\\arrayrulecolor{white}") %>%
  kableExtra::row_spec(row = 3 ,extra_latex_after = "\\arrayrulecolor{black}")

# Fit the parameteric model
options("contrasts" = c("contr.sum","contr.poly"))
endModel <- aov(sales ~ design, data = endCaps)

# QQ Plot
a <- car::qqPlot(
  x = endModel$residuals,
  distribution = "norm",
  envelope = 0.93,
  ylab = "% Increase in Sales",
  pch = 20
)

# Scatterplot for Homoscedasticity
stripchart(
  sales ~ design,
  data = endCaps,
  xlab = "End-cap Design",
  ylab = "Precent Increase in Sales",
  vertical = TRUE,
  pch = 19
)

# Create ANOVA Table
paraEndCap <-parameters::model_parameters(
  endModel, omega_squared = "raw",
  eta_squared = "raw", epsilon_squared = TRUE)
options(knitr.kable.NA= "")
knitr::kable(
  paraEndCap,
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Omega Sq.", "Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Table for End-Cap Design Study",
  align = c('l',rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position") %>%
  kableExtra::row_spec( row = 0, extra_latex_after = "\\arrayrulecolor{black}") %>%
  kableExtra::row_spec(row = 1, extra_latex_after = "\\arrayrulecolor{white}") %>%
  kableExtra::row_spec(row = 2 ,extra_latex_after = "\\arrayrulecolor{black}") %>%
  kableExtra::footnote(
    general = "Computer rounding has made the p-value look like zero.",
    footnote_as_chunk = T)

# Store treatment effects
designEffects <- dummy.coef(endModel)

# Post Hoc with Benjamini & Hochberg
pairwiseEnd <- pairwise.t.test(
  endCaps$sales, endCaps$design,
  p.adjust.method = "BH")

options(knitr.kable.NA= "")
knitr::kable(
  pairwiseEnd$p.value,
  digits = 4,
  caption = paste("Post Hoc",
                  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                       pairwiseEnd$p.adjust.method,
                       perl = TRUE),
                  "Comparisons"),
  align = rep('c', nrow(pairwiseEnd$p.value))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")

# Load Helper Functions
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Create Table
knitr::kable(
  anova.PostHoc(endModel),
  digits = 3,
  caption = "Post Hoc Comparison Effect Sizes",
  col.names = c("Pairwise Comparison","Cohen's d", "Hedge's g",
                "Prob. Superiority"),
  align = 'lccc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position") %>%
  kableExtra::row_spec( row = 0, extra_latex_after = "\\arrayrulecolor{black}") %>%
  kableExtra::row_spec(row = 1, extra_latex_after = "\\arrayrulecolor{white}") %>%
  kableExtra::row_spec(row = 3 ,extra_latex_after = "\\arrayrulecolor{black}")