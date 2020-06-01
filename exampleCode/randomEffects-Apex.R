# Apex Enterprises Example
# Data consist of the ratings of employment candidates by five
# personnel officers. HR selected the officers via lottery and
# assigned four candidates to each officer via second lottery for
# total of 20 candidates.

# Load Packages
packages <- c("tidyverse", "hasseDiagram","knitr",
              "kableExtra", "car", "parameters",
              "DescTools", "ggplot2", "lattice",
              "psych", "lme4")
lapply(packages, library, character.only = TRUE)

apex <- data.frame(
  officer = sort(c(rep(LETTERS[1:5], 4))),
  score = c(
    76, 65, 85, 74,
    59, 75, 81, 67,
    49, 63, 61, 46,
    74, 71, 85, 89,
    66, 84, 80, 79
  )
)

# Visualize the Model
apexLabels <- c("1 Grand Mean 1",
                "5 (Personnel Officer) 4",
                "20 (Candidates/Error) 15")
apexMat <- matrix(data = F, nrow = 3, ncol = 3)
apexMat[1, c(2:3)] = apexMat[2, 3] = T
hasseDiagram::hasse(apexMat, apexLabels)

# Fit Models
options("contrasts" = c("contr.sum","contr.poly"))
apexFE <- aov(score ~ officer, data = apex)
apexRE <- lme4::lmer(
  score ~ (1|officer),
  data = apex,
  REML = TRUE)

# Check Assumptions
a <- car::qqPlot(
  x = resid(apexRE),
  distribution = "norm",
  envelope = 0.92,
  ylab = "score",
  pch = 20,
  main = "Residuals"
)
b <- car::qqPlot(
  x = lme4::ranef(apexRE)$officer[, "(Intercept)"],
  distribution = "norm",
  envelope = 0.92,
  ylab = "score",
  pch = 20,
  main = "Random Effects"
)
plot(apexRE,
     pch = 20,
     xlab = "Fitted Values",
     ylab = "Residuals")

# Omnibus Test Results
options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    apexFE, omega_squared = "raw",
    eta_squared = "raw", epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Omega Sq.", "Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Table for Apex Personnel Officer Study",
  align = c('l',rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

# Get Point Estimates
points <- c(
  lme4::fixef(apexRE),
  attr(summary(apexRE)$varcor$officer, "stddev"),
  attr(summary(apexRE)$varcor, "sc")
  )

# Get Confidence Intervals
intervals <- confint(apexRE, level=0.979, oldNames = FALSE)

# Make a data table
apexEst <- as.data.frame(intervals)
apexEst$Point.Est <- c(NA, NA, NA)
apexEst[3, "Point.Est"] <- points[1]
apexEst[1, "Point.Est"] <- points[2]
apexEst[2, "Point.Est"] <- points[3]

apexEst <- apexEst %>%
  dplyr::select("Point.Est", everything())

row.names(apexEst) <- c("Treatment Standard Deviation",
                          "Residual Standard Deviation",
                          "Grand Mean")
knitr::kable(
  apexEst,
  digits = 3,
  caption = "Point Estimates and 92% Sidak Adj. Confidence Bounds",
  align = c('l',rep('c',3))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")