# Gummy Bears, Spring 2024 ----
# Load packages ----
packages <- c("tidyverse", "googlesheets4", "hasseDiagram", "car",
              "parameters", "knitr", "kableExtra")

invisible(lapply(
  X = packages,
  FUN = library,
  character.only = TRUE,
  quietly = TRUE
))

options(knitr.kable.NA = "")
options(contrasts = c("contr.sum", "contr.poly"))

source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")

# Load and Clean Data -----
gs4_deauth()
bearsData <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1xEo4PaswdVAK3F-pZ4nKuSyDgAqal-COqHaRWtJsdtg/edit?usp=sharing"
)

## Drop Columns that were improperly added ----
bearsData <- bearsData[, 1:6]


## Fix Data Types ----
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

# Create Hasse Diagram ----
## Section 2 ----
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

## Section 3 ----
#' Section 3 has an important design issue to acknowledge:
#' The High Back condition was not correctly configured, instead replicating the 
#' High Front condition. This is not easily addressed as the missing combination
#' results in a space of non-unique solutions.
modelLabels <- c("1 Launch Bears 1", "6 Teams 5", "3 Angle 2", "2 Position 1",
                 "5 Angle X Position 1", "36 (Spoonapults) 26")
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

# Create new data sets ----

## Create Spoonapult level Data ----
spoonapultsData <- bearsData %>%
  group_by(Section, Team, Angle, Position) %>%
  summarize(
    median = median(Distance, na.rm = TRUE),
    sam = mean(Distance, na.rm = TRUE),
    groupOrder = (sum(Order) + 10) / 25,
    .groups = "drop"
  )

spoons_sec2 <- spoonapultsData %>%
  filter(Section == "Section 2")

spoons_sec3 <- spoonapultsData %>%
  filter(Section == "Section 3")

#' From this point, I'm going to work with the two sections' data separately
#' to account for the missing cell (combination) in Section 3.

# Explore the Data ----
#' This is currently omitted.

# Fit the Model ----
spoonapultModel2 <- aov(
  formula = sam ~ Team + Angle*Position,
  data = spoons_sec2
)

# Assess Assumptions ----
## Gaussian Residuals ----
car::qqPlot(
  x = residuals(spoonapultModel2), 
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm/bear)"
)
#' The Gaussian Assumption looks satisfied.

## Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(spoonapultModel2),
    fitted = fitted.values(spoonapultModel2)
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_smooth(
    formula = y ~ x,
    method = stats::loess,
    method.args = list(degree = 1),
    se = FALSE,
    linewidth = 0.5
  ) +
  theme_bw() +
  xlab("Fitted values (cm/bear)") +
  ylab("Residuals (cm/bear)")

#' The homoscedasticity assumption is on the edge between satisfied and questionable

## Independence of Observations ----
#' Here we want to think about any potential threats that might have come
#' up during the enactment of the study.
#' 
#' For Index Plots, we will want to work inside each team.

### Team Level Index Plots Using Observations ----
bearsData %>%
  filter(Section == "Section 2") %>%
  ggplot(
    mapping = aes(
      x = Order,
      y = Distance
    )
  ) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_bw() +
  xlab("Measurement order") +
  ylab("Distance (cm/bear)") +
  facet_wrap(
    facets = vars(Team),
    scales = "fixed"
  )

### Team Levels Index Plots Using Summary Values ----
tempSec2 <- spoons_sec2
tempSec2$residuals <- residuals(spoonapultModel2)
ggplot(
  data = tempSec2,
  mapping = aes(
    x = groupOrder,
    y = residuals
  )
) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_bw() +
  xlab("Measurement order") +
  ylab("Residuals (cm/bear)") +
  facet_wrap(
    facets = vars(Team),
    scales = "fixed"
  )

### Interaction Between Block and Factors ----
#### Team and Angle ----
ggplot2::ggplot(
  data = spoons_sec2,
  mapping = aes(
    x = Team,
    y = sam,
    color = Angle,
    shape = Angle,
    linetype = Angle,
    group = Angle
  )
) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  ggplot2::theme_bw() +
  xlab("Team (block)") +
  ylab("SAM Distance (cm/bear)") +
  labs(color = "Angle", linetype = "Angle", shape = "Angle") +
  theme(
    legend.position = "right"
  ) +
  scale_color_manual(values = boastUtils::psuPalette)

#### Team and Position ----
ggplot2::ggplot(
  data = spoons_sec2,
  mapping = aes(
    x = Team,
    y = sam,
    color = Position,
    shape = Position,
    linetype = Position,
    group = Position
  )
) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  ggplot2::theme_bw() +
  xlab("Team (block)") +
  ylab("SAM Distance (cm/bear)") +
  labs(color = "Position", linetype = "Position", shape = "Position") +
  theme(
    legend.position = "right"
  ) +
  scale_color_manual(values = boastUtils::psuPalette)

#### Team and Angle:Position ----
ggplot2::ggplot(
  data = spoons_sec2,
  mapping = aes(
    x = Team,
    y = sam,
    color = Angle:Position,
    shape = Angle:Position,
    linetype = Angle:Position,
    group = Angle:Position
  )
) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  ggplot2::theme_bw() +
  xlab("Team (block)") +
  ylab("SAM Distance (cm/bear)") +
  labs(color = "Treatment", linetype = "Treatment", shape = "Treatment") +
  theme(
    legend.position = "right"
  ) +
  scale_color_manual(values = boastUtils::psuPalette)

#' Given the potential interaction between team and angle, as well as
#' team and position, we might need to proceed cautiously and potentially look
#' at alternative models.

## Alternative Models ----
### Option 1: Full Three-way Model ----
#' The first potential alternative model is to elevate our block, Team, to
#' a factor and fit a full factorial model with three factors. Unfortunately,
#' this model will not work with our data as we will have zero degrees of 
#' freedom for the residual screen.

### Option 2: Drop the Block ----
#' The second potential alternative model is to drop the block entirely and just
#' look at a two-way factorial model. This will mean that the block effects will
#' be confounded with the other terms in our model.

spoonsModel2_noBlock <- aov(
  formula = sam ~ Angle*Position,
  data = spoons_sec2
)

#### Gaussian Assumption ----
car::qqPlot(
  x = residuals(spoonsModel2_noBlock), 
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm/bear)"
)

#### Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(spoonsModel2_noBlock),
    fitted = fitted.values(spoonsModel2_noBlock)
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_smooth(
    formula = y ~ x,
    method = stats::loess,
    method.args = list(degree = 1),
    se = FALSE,
    linewidth = 0.5
  ) +
  theme_bw() +
  xlab("Fitted values (cm/bear)") +
  ylab("Residuals (cm/bear)")

### Option 3: Three-way Factorial Model giving up the three-way interaction ----

spoonsModel2_alt3 <- aov(
  formula = sam ~ Team + Angle + Position +
    Team:Angle + Team:Position + Angle:Position,
  data = spoons_sec2
)

#### Gaussian Assumption ----
car::qqPlot(
  x = residuals(spoonsModel2_alt3), 
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (cm/bear)"
)

#### Homoscedasticity ----
ggplot(
  data = data.frame(
    residuals = residuals(spoonsModel2_alt3),
    fitted = fitted.values(spoonsModel2_alt3)
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_smooth(
    formula = y ~ x,
    method = stats::loess,
    method.args = list(degree = 1),
    se = FALSE,
    linewidth = 0.5
  ) +
  theme_bw() +
  xlab("Fitted values (cm/bear)") +
  ylab("Residuals (cm/bear)")

# Omnibus Results ----

## Original ----
parameters::model_parameters(
  model = spoonapultModel2,
  effectsize_type = c("eta", "omega", "epsilon"),
  type = 3, 
  drop = "(Intercept)", 
  verbose = FALSE
) %>%
  dplyr::mutate(
    p = ifelse(
      test = is.na(p),
      yes = NA,
      no = pvalRound(p)
    )
  ) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for Spoonapults, Team as Block",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )

## Using Option 2 ----
parameters::model_parameters(
  model = spoonsModel2_noBlock,
  effectsize_type = c("eta", "omega", "epsilon"),
  type = 3, 
  drop = "(Intercept)", 
  verbose = FALSE
) %>%
  dplyr::mutate(
    p = ifelse(
      test = is.na(p),
      yes = NA,
      no = pvalRound(p)
    )
  ) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for Spoonapults, No Block",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )


## Using Option 3 ----
parameters::model_parameters(
  model = spoonsModel2_alt3,
  effectsize_type = c("eta", "omega", "epsilon"),
  type = 3, 
  drop = "(Intercept)", 
  verbose = FALSE
) %>%
  dplyr::mutate(
    p = ifelse(
      test = is.na(p),
      yes = NA,
      no = pvalRound(p)
    )
  ) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Eta Sq.", "Partial Omega Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for Spoonapults, No 3-way Interaction",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )
