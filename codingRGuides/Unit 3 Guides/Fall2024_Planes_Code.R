# Load packages ----
library(openxlsx)
library(tidyverse)

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
