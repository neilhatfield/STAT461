# Gummy Bears, Spring 2024 ----
## Load packages ----
packages <- c("tidyverse", "googlesheets4", "hasseDiagram", "car",
              "parameters", "knitr", "kableExtra")

invisible(lapply(
  X = packages,
  FUN = library,
  character.only = TRUE,
  quietly = TRUE
))

## Load and Clean Data -----
gs4_deauth()
bearsData <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1xEo4PaswdVAK3F-pZ4nKuSyDgAqal-COqHaRWtJsdtg/edit?usp=sharing"
)

### Drop Columns that were improperly added ----
bearsData <- bearsData[, 1:6]

### Fix Data Types ----
bearsData$Team <- as.character(bearsData$Team)
bearsData$Team <- as.factor(bearsData$Team)
bearsData$Angle <- factor(
  x = bearsData$Angle,
  levels = c("Flat", "Low", "High") # I want a particular ordering
)
bearsData$Position <- factor(
  x = bearsData$Position,
  levels = c("Front", "Back") # I want a particular ordering
)
bearsData$Order <- as.character(bearsData$Order)
bearsData$Order <- gsub(
  pattern = "GB", 
  replacement = "",
  x = bearsData$Order
)
bearsData$Order <- gsub(
  pattern = "G", 
  replacement = "",
  x = bearsData$Order
)
bearsData$Order <- na_if(x = bearsData$Order, y = "NULL")
bearsData$Order <- parse_number(bearsData$Order)

names(bearsData)[which(grepl(pattern = "(cm)", x = names(bearsData)))] <- "Distance"
bearsData$Distance <- as.character(bearsData$Distance)
bearsData$Distance <- parse_number(bearsData$Distance)

## Create Spoonapult level Data ---
spoonapultsData <- bearsData %>%
  group_by(Section, Team, Angle, Position) %>%
  summarize(
    median = median(Distance, na.rm = TRUE),
    sam = mean(Distance, na.rm = TRUE)
  )

## Create Hasse Diagram ----
modelLabels <- c("1 Launch Bears 1", "6 Teams 5", "3 Angle 2", "2 Position 1",
                 "6 Angle X Position 2", "36 (Spoonapults) 25")
modelMatrix <- matrix(
  data = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
           FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
           FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE,
           TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  nrow = 6,
  ncol = 6,
  byrow = FALSE
)
hasseDiagram::hasse(
  data = modelMatrix,
  labels = modelLabels
)
