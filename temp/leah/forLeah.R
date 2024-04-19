library(openxlsx)
library(tidyverse)
library(rvest)
library(chromote)

# Load Spreadsheet ----
links <- readWorkbook(
  # xlsxFile = "StudentWork/leah/leah.xlsx",
  xlsxFile = "temp/leah/leah.xlsx",
  colNames = FALSE
)
names(links) <- c("SchoolSport", "link")

newLinks <- links %>%
  separate_wider_delim(
    cols = SchoolSport,
    delim = " ",
    names = c("a", "b", "c", "sport"),
    too_few = "align_end",
    too_many = "merge"
  ) %>%
  unite(
    col = "school",
    a, b, c,
    sep = " ",
    na.rm = TRUE
  )

# Testing one page scrape of Live HTML via chromote ----
# temp1 <- read_html("https://iuhoosiers.com/sports/baseball/roster/2023") %>%
#   html_elements(css = "table") %>%
#   html_table()

ses <- chromote::ChromoteSession$new()
ses$Page$navigate("https://purduesports.com/sports/baseball/roster/2023")
ses$view()

test1 <- ses$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>%
  read_html() %>%
  html_elements(css = "table") %>%
  html_table()

## works for Indiana University; University of Minnesota
## University of Michigan; Northwestern; Ohio State University; Purdue University
## need xpath. Explore:html_elements(xpath = '//*[@id="listPanel"]/div[1]/div[1]/div[1]')

# Create a function for vectorization ----
grabTables <- function(index) {
  pageTables <- read_html(x = newLinks$link[index]) %>%
    html_elements(css = "table") %>%
    html_table()

  outObj <- list(
    school = newLinks$school[index],
    sport = newLinks$sport[index],
    tables = pageTables
  )

  return(outObj)
}

# Grab initial set of rosters ----
initialList <- lapply(
  X = 1:nrow(newLinks),
  FUN = grabTables
)

# Identify problem schools and target tables ----
problemSchools <- c("Indiana University", "University of Michigan",
                    "University of Minnesota", "Northwestern University",
                    "Ohio State University", "Purdue University")

## Potential Workaround
## works for Indiana University; University of Minnesota
### Indiana is 4:6; UMN is 19:21
ses <- chromote::ChromoteSession$new()

grabTables2 <- function(index) {
  ses$Page$navigate(newLinks$link[index])
  pageTables <- ses$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>%
    read_html() %>%
    html_elements(css = "table") %>%
    html_table()
  
  outObj <- list(
    school = newLinks$school[index],
    sport = newLinks$sport[index],
    tables = pageTables
  )
  
  return(outObj)
}


ses$Page$navigate("https://purduesports.com/sports/baseball/roster/2023")
ses$view()

test1 <- ses$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>%
  read_html() %>%
  html_elements(css = "table") %>%
  html_table()

targetTables <- c(
  3, 3, 4,
  0, 0, 0,
  1, 1, 1,
  3, 3, 3,
  0, 0, 0,
  3, 3, 3,
  0, 0, 0,
  1, 1, 1,
  0, 0, 0,
  0, 0, 0,
  3, 3, 3,
  0, 0, 0,
  3, 3, 3,
  3, 3, 3
)

# Extract and process rosters ----
getRosters <- function(index) {
  school <- initialList[[index]]$school
  sport <- initialList[[index]]$sport
  if (targetTables[index] > 0) {
    roster <- initialList[[index]]$tables[[targetTables[index]]]
    roster$school <- school
    roster$sport <- sport

  } else {
    roster <- data.frame(
      school =  school,
      sport = sport
    )
  }

  return(roster)
}

rosterSet <- lapply(
  X = 1:length(targetTables),
  FUN = getRosters
)

## Test Run ----
noms <- c("Name", "Full Name")
hauteurs <- c("Height", "Ht.")
poids <- c("Weight", "Wt.")

parseRosters <- function(index) {
  roster <- rosterSet[[index]]

  if (nrow(roster) == 1) {
    roster$Name <- NA
    roster$Height <- NA
    roster$Weight <- NA
  } else {
   roster <- roster %>%
      dplyr::select(school, sport, any_of(noms), any_of(hauteurs), any_of(poids))
  }

  names(roster) <- c("School", "Sport", "Name", "Height", "Weight")

  roster <- roster %>%
    mutate(
      across(.cols = everything(), .fns = as.character)
      )

  return(roster)
}

parsedRosters <- lapply(
  X = 1:length(rosterSet),
  FUN = parseRosters
)

players <- bind_rows(parsedRosters)

