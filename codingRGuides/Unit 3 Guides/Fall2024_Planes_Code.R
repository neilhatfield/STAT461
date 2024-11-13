# Load packages ----
library(openxlsx)
library(tidyverse)
library(parameters)
library(knitr)
library(kableExtra)
library(BrailleR)

# Load Data ----
planesRaw <- readWorkbook(
  xlsxFile = "https://raw.github.com/neilhatfield/STAT461/main/dataFiles/paper_airplane_data_Fall24.xlsx",
  startRow = 3
)

## Clean the data ----
names(planesRaw)[1] <- "Throw"

planesData <- planesRaw %>%
  pivot_longer(
    cols = !Throw,
    names_to = "Design",
    names_transform = list(Design = as.factor),
    values_to = "Distance"
  )

source("https://raw.github.com/neilhatfield/STAT461/main/rScripts/ANOVATools.R")

planeScreens <- anovaScreens(
  dataFrame = planesData,
  response = "Distance",
  factor = "Design"
)

planeScreens <- planeScreens %>%
  mutate(
    postS1 = Distance - Screen1.Action,
    .after = Screen1.Action
  )

# Fit Model ----
planeModel <- aov(
  formula = Distance ~ Design,
  data = planesData
)

# Assess Assumptions ----
## Gaussian ----
car::qqPlot(
  x = planeModel$residuals,
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (inches)",
  main = "QQ Plot for Fall 2024 Paper Plane Study"
)
psych::skew(planeModel$residuals)
psych::kurtosi(planeModel$residuals)

## Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = planeModel$residuals,
    fitted = planeModel$fitted.values
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  theme_bw() +
  labs(
    x = "Fitted values (inches)",
    y = "Residuals (inches)",
    title = "Strip Chart for Fall 2024 Paper Plane Study"
  )

# Independent Observations ----
ggplot(
  data = data.frame(
    residuals = planeModel$residuals,
    index = 1:length(planeModel$residuals)
  ),
  mapping = aes(x = index, y = residuals)
) +
  geom_point(size = 1.5) +
  geom_line() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    x = "Measurement order one",
    y = "Residuals (inches)",
    title = "Index Plot A for Fall 2024 Paper Plane Study"
  )
round(car::durbinWatsonTest(planeModel)$dw, digits = 2)

cbind(planesData, residuals = planeModel$residuals) %>%
  arrange(Design, Throw) %>%
  mutate(index = row_number()) %>%
  ggplot(
    mapping = aes(x = index, y = residuals)
  ) +
    geom_point(size = 1.5) +
    geom_line() +
    theme_bw() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "red"
    ) +
    labs(
      x = "Measurement order two",
      y = "Residuals (inches)",
      title = "Index Plot B for Fall 2024 Paper Plane Study"
    )

planeModelB <- planesData %>%
  arrange(Design, Throw) %>%
  aov(
    formula = Distance ~ Design
  )
round(car::durbinWatsonTest(planeModelB)$dw, digits = 2)

# ANOVA Table ----
parameters::model_parameters(
  model = planeModel,
  es_type = c("eta", "omega", "epsilon")
) %>%
  knitr::kable(
    digits = 4,
    col.names = c(
      "Source", "SS", "df", "MS", "F", "p-value",
      "Eta Sq.", "Omega Sq.", "Epsilon Sq."),
    caption = "Modern ANOVA Table for Song Knowledge Study",
    booktabs = TRUE,
    align = c("l", rep("c", 8))
  ) %>%
  kableExtra::kable_styling(
    font_size = 12,
    latex_options = c("HOLD_position")
  )
