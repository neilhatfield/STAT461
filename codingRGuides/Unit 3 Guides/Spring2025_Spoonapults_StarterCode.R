# Data Wrangling Spring 2025 Spoonapult Data ----
#' This code is for loading and wranging the data from the Spring 2025
#' Spoonapult study from Stat 461.

# Install Packages (if missing) ----
## Delete the hashtag at the start of each line and then submit the commands in R
# install.packages("openxlsx")
# install.package("tidyverse")

# Load Packages ----
library(openxlsx)
library(tidyverse)

# Load raw data ----
rawSpoons <- read.xlsx(
  xlsxFile = "https://raw.githubusercontent.com/neilhatfield/STAT461/main/dataFiles/spoonapults_Spring2025.xlsx"
)

# Wrangle Spoonapult Data ----
#' Data are not tidy; treatments are embedded in a single column
#' instead of separate columns for each factor.
#' Factor levels have mixture of capital and lower case first letters
#' The order of the entries in the treatment cells is not consistent
#' The two launches have some cells with unit of measurement included.
#' The two launches need to be aggregated to a single value for each spoonapult

spoonapultData <- rawSpoons %>%
  mutate(
    Treatment = str_squish(str_to_lower(Treatment))
  ) %>%
  mutate(
    position = case_when(
      grepl(pattern = "back", Treatment) ~ "back",
      grepl(pattern = "front", Treatment) ~ "front",
      .default = "error"
    ),
    angle = case_when(
      grepl(pattern = "flat", Treatment) ~ "flat",
      grepl(pattern = "low", Treatment) ~ "low",
      grepl(pattern = "high", Treatment) ~ "high",
      .default = "error"
    ),
    .after = Treatment
  ) %>%
  mutate(
    Group = paste("Team", Group),
    first = parse_number(first),
    second = parse_number(second)
  ) %>%
  dplyr::select(!Treatment) %>%
  pivot_longer(
    cols = c(first, second),
    names_to = "shot",
    values_to = "distance"
  ) %>%
  group_by(Spoonapult, Group, position, angle) %>%
  summarize(
    sam_distance = mean(distance),
    .groups = "drop"
  )

## Set Factor Data Types ----
spoonapultData$Group <- as.factor(spoonapultData$Group)
spoonapultData$position <- as.factor(spoonapultData$position)
spoonapultData$angle <- factor(x = spoonapultData$angle, levels = c("flat", "low", "high"))  
