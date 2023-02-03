# Load Packages and set global options
packages <- c("tidyverse", "hasseDiagram","knitr",
              "kableExtra", "car", "parameters",
              "DescTools", "ggplot2")
lapply(packages, library, character.only = TRUE)
options("contrasts" = c("contr.sum","contr.poly"))

# Get Data
hardness <- data.frame(
  tip = sort(rep(LETTERS[1:4], 4)),
  blank = rep(letters[23:26], 4),
  depth = c(
    9.3, 9.4, 9.6, 10.0,
    9.4, 9.3, 9.8, 9.9,
    9.2, 9.4, 9.5, 9.7,
    9.7, 9.6, 10.0, 10.2)
)

# Make a Hasse Diagram
hardLabels <- c("1 Grand Mean 1", "4 Blank 3",
                    "4 Tip 3", "16 (Tests/Error) 9")
hardMat <- matrix(data = F, nrow = 4, ncol = 4)
hardMat[1, c(2:4)] = hardMat[2, 4] = hardMat[3, 4] = T
hasseDiagram::hasse(hardMat, hardLabels)

# Fit the Model
hardModel <- aov(depth ~ tip + blank, data = hardness)

# Check Asumptions
## Normality of Residuals

a <- car::qqPlot(
  x = hardModel$residuals,
  distribution = "norm",
  envelope = 0.97,
  ylab = "Depth (mm)",
  pch = 19
)

## Homoscedasticity on Residuals
plot(hardModel, which = 1)

## Interaction
ggplot2::ggplot(data = hardness,
                mapping = aes(x = tip,
                              y = depth,
                              color = blank,
                              group = blank)) +
  ggplot2::geom_point(size=2) +
  ggplot2::geom_line(size=1) +
  ggplot2::theme_bw() +
  xlab("Tip Used") +
  ylab("Depth (mm)") +
  labs(color = "Testing Blank")

# Omnibus test
anova(hardModel)

# Check Estimates
dummy.coef(hardModel)

# Nice Looking Table
options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    hardModel, omega_squared = "partial",
    eta_squared = "partial", epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Partial Omega Sq.", "Partial Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Table for Hardness Study",
  align = c('l',rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

# Reletive Efficiency of Block
block.RelEff(hardModel, "blank", "tip")

# Post Hoc Analysis
hardness.PH <- TukeyHSD(hardModel)

# Nice Looking Table
knitr::kable(
  hardness.PH$tip,
  digits = 3,
  caption = "Post Hoc Tukey HSD Comparisons",
  col.names = c("Difference", "Lower Bound",
                "Upper Bound", "Adj. p-Value"),
  align = 'cccc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")
