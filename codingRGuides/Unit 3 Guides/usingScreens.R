# Load Packages ----
packages <- c("tidyverse", "openxlsx", "knitr", "kableExtra",
              "car", "parameters")
invisible(
  lapply(
    X = packages,
    FUN = library,
    character.only = TRUE,
    quietly = TRUE
  )
)

# Load Data ----
plane2 <- readWorkbook(
  xlsxFile = "https://raw.github.com/neilhatfield/STAT461/main/dataFiles/PaperAirplanes_Sp24_Sec2.xlsx"
)
plane3 <- readWorkbook(
  xlsxFile = "https://raw.github.com/neilhatfield/STAT461/main/dataFiles/PaperAirplanes_Sp24_Sec3.xlsx"
)

# Clean Data ----
names(plane2) <- c("design", "folder", "thrower", "distance")
names(plane3) <- c("design", "distance", "notes")

plane2 <- plane2 %>%
  mutate(
    across(where(is.character), factor)
  )

plane3$design <- gsub(pattern = "\\*", replacement = "", x = plane3$design)

plane3$design <- as.factor(plane3$design)

# Apply Screens ----
## Load Tools ----
source("https://raw.github.com/neilhatfield/STAT461/main/rScripts/ANOVATools.R")

## Sec. 2
plane2_Screens <- anovaScreens(
  dataFrame = plane2,
  response = "distance",
  factor = "design"
)

## Sec. 3
plane3_Screens <- anovaScreens(
  dataFrame = plane3,
  response = "distance",
  factor = "design"
)
