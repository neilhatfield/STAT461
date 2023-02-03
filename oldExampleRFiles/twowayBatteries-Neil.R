# Factorial Example-Twoway-Battery Life

# Load packages
packages <- c("tidyverse", "hasseDiagram","knitr",
              "kableExtra", "car", "ggplot2",
              "parameters", "emmeans")
lapply(packages, library, character.only = TRUE)

# Load Helper Functions
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Set Global Options
options("contrasts" = c("contr.sum","contr.poly"))

# Hasse Diagram-On Your Own
batLabels <- c("1 GM 1", "3 Plate 2", "3 Temperature 2",
               "9 P X T 4", "36 (Batteries/Error) 25")
batMat <- matrix(data = F, nrow = 5, ncol = 5)
batMat[1, c(2:5)] = batMat[c(2:3), c(4:5)] = batMat[4, 5] = T
hasseDiagram::hasse(batMat, batLabels)

# Get Data
battery <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/batteryLife.dat",
  header = TRUE, sep = ",")

## Set Factors
battery$material <- as.factor(battery$material)
battery$temperature <- as.factor(battery$temperature)

# Explore the Data--On Your Own
## Data Visualizations
boxplot(life ~ temperature * material, data = battery,
        ylab = "Life (hrs)",
        xlab = "Temp (ºF).Material")

## Descriptive Statistics

# Two-way ANOVA Model
## You have two options for specifying the formula
## Option 1: y ~ A + B + A:B
## Option 2: y ~ A*B
## They are equivalent to each other.

batteryModel <- aov(life ~ material + temperature + temperature:material,
                    data = battery)

# Assumptions

## Normality
a <- car::qqPlot(
  x = batteryModel$residuals,
  distribution = "norm",
  envelope = 0.97,
  ylab = "Life (hours)",
  pch = 19
)

## Homoscedasticity
plot(batteryModel, which = 1, pch = 19)

## Interaction Plot-Using interaction.plot
interaction.plot(x.factor = battery$temperature,
                 trace.factor = battery$material,
                 response = battery$life,
                 fun = mean,
                 type = "b",
                 col = c("black","red","blue"),
                 pch = c(19, 17, 15),
                 fixed = TRUE,
                 legend = TRUE,
                 xlab = "Temperature (ºF)",
                 ylab = "Life (hours)",
                 trace.label = "Material")

## Interaction Plot-Using ggplot2
ggplot2::ggplot(data = battery,
                mapping = aes(x = temperature,
                              y = life,
                              color = material,
                              group = material)) +
  ggplot2::stat_summary(fun = "mean", geom = "point") +
  ggplot2::stat_summary(fun = "mean", geom = "line") +
  ggplot2::geom_jitter(width = 0.1, height = 0.1, shape = 5) +
  ggplot2::theme_bw() +
  xlab("Temperature (ºF)") +
  ylab("Life (hours)") +
  labs(color = "Material")

# ANOVA Table
anova(batteryModel)

options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    batteryModel, omega_squared = "partial",
    eta_squared = "partial", epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Partial Omega Sq.", "Partial Eta Sq.", "Epsilon Sq."),
  caption = "ANOVA Table for Battery Life Study",
  align = c('l',rep('c',8))
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

# Post Hoc-Use the EMMeans package
## The emmeans package will use either
## Tukey's Adjustment for multiple comparisons OR
## Sidak's for simultaneous confidence intervals.

### Interaction Plot--Additional Method
emmeans::emmip(batteryModel, material ~ temperature)

### Pairwise Comparisons
postTemp <- emmeans::emmeans(batteryModel,
                 pairwise ~ temperature | material,
                 adjust = "tukey",
                 level = 0.9)
postMat <- emmeans::emmeans(batteryModel,
                             pairwise ~ material | temperature,
                            adjust = "sidak",
                            level = 0.9)

summary(postTemp)
confint(postTemp, level = 0.9)

summary(postMat)
confint(postMat, level = 0.9)

### Cohen's d
tempEMM <- emmeans::emmeans(batteryModel, "temperature")
cohenTemp <- emmeans::eff_size(tempEMM,
                  sigma = sigma(batteryModel),
                  edf = df.residual(batteryModel))

matEMM <- emmeans::emmeans(batteryModel, "material")
cohenMat <- emmeans::eff_size(matEMM,
                  sigma = sigma(batteryModel),
                  edf = df.residual(batteryModel))


pnas <- matrix(data = F, nrow = 9, ncol = 9)
pnas[1, c(2:9)] = pnas[c(2:3), c(5:9)] = pnas[4, 9] = pnas[5, c(6:9)] = pnas[c(6:8), 9] = T
pnasLabels <- c("1 GM 1","2 (Instructor) 1","4 Semester 3",
                "2 Gender 1", "8 (Treatment) 3", "Cov: FCI 1",
                "Cov: CLASS 1","Cov: Midterms 1",
                "149 (Student/Error) 137")
hasseDiagram::hasse(data = pnas, labels = pnasLabels)