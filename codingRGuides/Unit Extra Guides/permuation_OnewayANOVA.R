# Permutation Test for One-way ANOVA ----
# Load Packages ----
library(tidyverse)
library(permute)
library(openxlsx)
library(iterpc)

# Set Global Options ----
options(contrasts = c("contr.sum", "contr.poly"))

# Load and Clean Data ----
planeData <- read.xlsx(
  xlsxFile = "https://raw.githubusercontent.com/neilhatfield/STAT461/main/dataFiles/airplanes_Spring25.xlsx"
)

planeData$design <- as.factor(planeData$design)

# Get Observed F Ratio Value ----
planeModel <- aov(
  formula = distance_in ~ design,
  data = planeData
)

obs.F <- anova(planeModel)$`F value`[1]

# Define Function for Getting Permutation F Ratio Values ----
perm.F <- function(data, response, factor) {
  perm <- shuffle(nrow(data))
  shuffledData <- data.frame(
    response = data[response],
    factor = data[[factor]][perm]
  )
  model <- aov(
    formula = as.formula(paste(response, "~", "factor")),
    data = shuffledData
  )
  f <- anova(model)$`F value`[1]
  return(f)
}

# Define Freedman-Diaconis Rule ----
fdRule <- function(x) {
  return(
    ifelse(
      test = IQR(x) == 0,
      yes =  0.1,
      no =  2 * IQR(x) / (length(x)^(1/3))
    )
  )
}

# Calculate Total Number of Combinations ----
multichoose(n = c(4,4,4))

# Carry Out Permutation ----
set.seed(461)
planePerm <- replicate(
  n = 999,
  expr = perm.F(
    data = planeData,
    response = "distance_in",
    factor = "design"
  )
)

## Add observed F ratio value ----
planePerm[length(planePerm) + 1] <- obs.F

# Make Histogram ----
ggplot(
  data = data.frame(
    `F` = planePerm,
    type = c(rep("perm", length(planePerm) - 1), "obs")
  ),
  mapping = aes(x = `F`)
) +
  geom_histogram(
    color = "black",
    fill = "blue",
    binwidth = fdRule,
    boundary = 0,
    closed = "left"
  ) +
  geom_vline(
    xintercept = obs.F,
    color = "red"
  ) +
  annotate(
    geom = "label",
    x = Inf,
    y = Inf,
    hjust = 1.01,
    vjust = 2,
    label = paste(
      "Freq ≥ Obs. F \n",
      "Abs. Freq. is", sum(planePerm >= obs.F), "\n",
      "Rel. Freq. is", paste0(round(sum(planePerm >= obs.F)/length(planePerm)*100, 2), "%")
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_bw() +
  labs(
    x = "Values of the F Ratio",
    y = "Frequency"
  )

