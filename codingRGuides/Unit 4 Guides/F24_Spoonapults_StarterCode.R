# Gummy Bear Analysis-Fall 2024 ----
## Initial Getting Started Code

# Load Packages -----
library(tidyverse)
library(openxlsx)

# Load Original Data ----
bears <- openxlsx::readWorkbook(
  xlsxFile = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/gummyBears_Fall2024.xlsx",
  sheet = 1,
  colNames = TRUE,
  rowNames = FALSE
)

## Clean Original Data ----
### 1) Inconsistent formatting of levels for both factors
### 2) Misspelled front position
### 3) Standardize Team Names
### 4) Set factors and block as factor data type
### 5) Set id columns to character data type

bears <- bears %>%
  mutate(
    Team = str_to_sentence(Team), # Resolves 3
    Angle = str_trim(str_to_sentence(Angle)), # Resolves 1
    Position = str_trim(str_to_sentence(Position)) # Resolves 1
  ) %>%
  mutate(
    Position = case_when(
      Position == "Friont" ~ "Front",
      .default = Position
    ) # Resolves 2
  ) %>%
  mutate(
    Team = as.factor(Team), # Resolves 4
    Spoonapult = as.character(Spoonapult), # Resolves 5
    Angle = factor(Angle, levels = c("Flat", "Low", "High")), # Resolves 4
    Position = as.factor(Position), # Resolves 4
    Bear = as.character(Bear) # Resolves 5
  )

# Create Experimental Unit Data Frame ----
spoonapults <- bears %>%
  group_by(Team, Angle, Position, Spoonapult) %>%
  summarize(
    SAM_in = mean(Distance_in),
    Med_in = median(Distance_in),
    SAM_cm = mean(Distance_cm),
    Med_cm = mean(Distance_cm),
    .groups = "drop"
  )
