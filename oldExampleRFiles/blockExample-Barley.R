# Load Packages and set global options
packages <- c("tidyverse", "hasseDiagram","knitr",
              "kableExtra", "car", "parameters",
              "DescTools", "ggplot2")
lapply(packages, library, character.only = TRUE)
options("contrasts" = c("contr.sum","contr.poly"))
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")


# Get Data
barley <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/barley.dat",
  header = TRUE, sep = ",")

names(barley)[4] <- "Order"


# Make a Hasse Diagram
barleyLabels <- c("1 Grand Mean 1", "4 Field 3",
                "4 Variety 3", "16 (Crop/Error) 9")
barleyMat <- matrix(data = F, nrow = 4, ncol = 4)
barleyMat[1, c(2:4)] = barleyMat[2, 4] = barleyMat[3, 4] = T
hasseDiagram::hasse(barleyMat, barleyLabels)

# Look at the data
ggplot2::ggplot(data = barley,
                mapping = aes(x = Order,
                              y = Yield,
                              color = Field,
                              shape = Treatment)) +
  ggplot2::geom_point( size = 3) +
  ggplot2::geom_path(aes(group=Field)) +
  ggplot2::theme_bw() +
  xlab("Planting/Havesting Order") +
  ylab("Yield (bushels per acre)")

# Fit the Model
barleyModel <- aov(Yield ~ Treatment + Field, data = barley)

# Check Asumptions-Properly
## Normality of Residuals
a <- car::qqPlot(
  x = barleyModel$residuals,
  distribution = "norm",
  envelope = 0.97,
  ylab = "Yield (bushels per arce)",
  pch = 19,
  col.lines = "red"
)

## Homoscedasticity on Residuals
plot(barleyModel, which = 1, pch = 19, col= "red")

# Ignoring the Block-Don't Do!
## Checking Homoscedasticity
test <- aov(Yield ~ Treatment, data= barley)
plot(test, which = 1, pch = 19, col= "blue")

stripchart(
  Yield ~ Treatment,
  data = barley,
  pch = 19,
  vertical = TRUE,
  ylab = "Yield (bushels per acre)",
  xlab = "Type of barley",
  col = "blue"
)

# Check for Interaction
ggplot2::ggplot(data = barley,
                mapping = aes(x = Treatment,
                              y = Yield,
                              color = Field,
                              group = Field)) +
  ggplot2::geom_point(size=2) +
  ggplot2::geom_line(size=1) +
  ggplot2::theme_bw() +
  xlab("Variety") +
  ylab("Yield (bushels per acre)") +
  labs(color = "Field")


# Omnibus test
anova(barleyModel)

# Check Estimates
dummy.coef(barleyModel)

# Nice Looking Table
options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    barleyModel, omega_squared = "partial",
    eta_squared = "partial", epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Partial Omega Sq.", "Partial Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Table for Barley Study",
  align = c('l',rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

# Reletive Efficiency of Block
block.RelEff(barleyModel, "Field", "Treatment")

# Post Hoc Analysis
barley.PH <- TukeyHSD(barleyModel,
                      which = "Treatment",
                      conf.level = 0.9)

# Nice Looking Table
knitr::kable(
  barley.PH$Treatment,
  digits = 3,
  caption = "Post Hoc Tukey HSD Comparisons",
  col.names = c("Difference", "Lower Bound",
                "Upper Bound", "Adj. p-Value"),
  align = 'cccc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12, latex_options = "HOLD_position")
