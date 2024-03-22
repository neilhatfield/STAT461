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

## Sec. 2 ----
plane2_Screens <- anovaScreens(
  dataFrame = plane2,
  response = "distance",
  factor = "design"
)

## Sec. 3 ----
plane3_Screens <- anovaScreens(
  dataFrame = plane3,
  response = "distance",
  factor = "design"
)

# Screen Calculations ----
## Sec. 3 ----
screenCalc3 <- plane3_Screens %>%
  dplyr::select(distance, contains("Screen")) %>%
  summarize(
    across(
      .cols = everything(),
      .fns = ~ sum(.x^2, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Source",
    values_to = "SS_0"
  ) %>%
  mutate(
    df = c(nrow(plane3_Screens), 1, 3, nrow(plane3_Screens) - 4),
    MS = case_when(
      Source == "distance" ~ NA_real_,
      grepl(pattern = "Action", x = Source) ~ NA_real_,
      .default = SS_0 / df
    )
  )

## Sec. 2 ----
screenCalc2 <- plane2_Screens %>%
  dplyr::select(distance, contains("Screen")) %>%
  summarize(
    across(
      .cols = everything(),
      .fns = ~ sum(.x^2, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Source",
    values_to = "SS_0"
  ) %>%
  mutate(
    df = c(nrow(plane2_Screens), 1, 3, nrow(plane2_Screens) - 4),
    MS = case_when(
      Source == "distance" ~ NA_real_,
      grepl(pattern = "Action", x = Source) ~ NA_real_,
      .default = SS_0 / df
    )
  )

# ANOVA Model Check ----
plane2Model <- aov(
  formula = distance ~ design,
  data = plane2,
  na.action = "na.omit"
)
anova(plane2Model)

plane3Model <- aov(
  formula = distance ~ design,
  data = plane3,
  na.action = "na.omit"
)
anova(plane3Model)

# Assumption Checking ----
## Sec. 2 ----
### Gaussian Residuals ----
car::qqPlot(
  x = plane2Model$residuals,
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (in)",
  main = "Sec. 2's Air Plane Study Residual QQ Plot"
)
psych::skew(plane2Model$residuals)
psych::kurtosi(plane2Model$residuals)

### Homoscedasicity ----
ggplot(
  data = data.frame(
    residuals = plane2Model$residuals,
    fitted = plane2Model$fitted.values
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  theme_bw() +
  labs(
    x = "Fitted values (in)",
    y = "Residuals (in)",
    title = "Sec. 2's Strip Chart for the Paper Airplane Study"
  )

### Independent Obs. ----
ggplot(
  data = data.frame(
    residuals = plane2Model$residuals,
    index = 1:length(plane2Model$residuals)
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
    x = "Measurement order",
    y = "Residuals (in)",
    title = "Sec. 2's Index Plot for the Paper Airplane Study"
  )
car::durbinWatsonTest(plane2Model)$dw

## Sec. 3 ----
### Gaussian Residuals ----
car::qqPlot(
  x = plane3Model$residuals,
  distribution = "norm",
  envelope = 0.90,
  id = FALSE,
  pch = 20,
  ylab = "Residuals (in)",
  main = "Sec. 3's Air Plane Study Residual QQ Plot"
)
psych::skew(plane3Model$residuals)
psych::kurtosi(plane3Model$residuals)

### Homoscedasicity ----
ggplot(
  data = data.frame(
    residuals = plane3Model$residuals,
    fitted = plane3Model$fitted.values
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  theme_bw() +
  labs(
    x = "Fitted values (in)",
    y = "Residuals (in)",
    title = "Sec. 3's Strip Chart for the Paper Airplane Study"
  )

### Independent Obs. ----
ggplot(
  data = data.frame(
    residuals = plane3Model$residuals,
    index = 1:length(plane3Model$residuals)
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
    x = "Measurement order",
    y = "Residuals (in)",
    title = "Sec. 3's Index Plot for the Paper Airplane Study"
  )
car::durbinWatsonTest(plane3Model)$dw
