# Fall 2023 Gummy Bears Data ---
library(tidyverse)
library(openxlsx)

bears <- openxlsx::readWorkbook(
  xlsxFile = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/gummyBears_Fall2023.xlsx",
  sheet = 1,
  colNames = TRUE,
  rowNames = FALSE
)

# Measurement Units Analysis ----
bears$angle <- as.factor(bears$angle)
bears$position <- as.factor(bears$position)
bears$team <- as.factor(bears$team)

bears <- bears %>%
  mutate(
    shifted_distance = case_when(
      position == "Front" ~ distance - 2.54*3,
      position == "Back" ~ distance - 2.54*33
    )
  )

model1 <- aov(
  formula = distance ~ angle*position,
  data = bears
)
plot(model1)
parameters::model_parameters(model1, effectsize_type = c("eta", "omega", "epsilon")) %>%
  kable(digits = 4) %>%
  kable_classic()

model1b <- aov(
  formula = shifted_distance ~ angle*position,
  data = bears
)
plot(model1b)
anova(model1b)

# Experimental Unit Analysis ----
bears2 <- bears %>%
  group_by(angle, position, team) %>%
  summarize(
    sam_dist = mean(distance, na.rm = TRUE),
    sam_shifted = mean(shifted_distance, na.rm = TRUE)
  )

model2 <- aov(
  formula = sam_dist ~ angle*position,
  data = bears2
)
plot(model2)
parameters::model_parameters(model2, effectsize_type = c("eta", "omega", "epsilon")) %>%
  kable(digits = 4) %>%
  kable_classic()

model2b <- aov(
  formula = sam_shifted ~ angle*position,
  data = bears2
)
plot(model2b)
anova(model2b)
