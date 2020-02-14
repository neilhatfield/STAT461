# Honey Demo 1
# We will use a Significance Level/Unusualness Threshold of 0.03

# Packages needed
packages <- c("hasseDiagram", "tidyverse",
              "knitr", "kableExtra", "car",
              "sjstats", "parameters")
lapply(packages, library, character.only = TRUE)

# Create Hasse Diagram for the experiment
{
  honeyLabels <- c("1 Grand Mean 1","3 Varietal 2", "9 (Beehives/Error) 6")
  honeyMat <- matrix(data = F, nrow = 3, ncol = 3)
  honeyMat[1, c(2:3)] = honeyMat[2, 3] = T
  hasseDiagram::hasse(data = honeyMat, labels = honeyLabels)
}

# Get Data
honey <- data.frame(
  Surplus = c(150, 50, 100, 85, 90, 95, 130, 50, 80),
  Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
)

# Check Assumptions on Original Data
{
  ## Normality--whole data set
  car::qqPlot(
    x = honey$Surplus,
    distribution = "norm",
    envelope = 0.97,
    ylab = "Surplus Honey (lbs)"
  )
  ## Normality--By group
  car::qqPlot(
    x = honey$Surplus,
    group = honey$Varietal,
    distribution = "norm",
    envelope = 0.93,
    ylab = "Surplus Honey (lbs)",
    glab = "Varietal"
  )
  ## Homoscedasticity
  stripchart(Surplus ~ Varietal,
             vertical = TRUE,
             pch = 20,
             data = honey)
  ## Independence
  ## Provided we know that order of measurement
  plot(honey$Surplus, type = "b", ylab = "Surplus Honey (lbs)")
}

# Fit the Model
model1 <- aov(Surplus ~ Varietal, data = honey, na.action = "na.omit")

# Checking Assumptions on the Residuals
{
  # Calling plot(model1) will display all of the plots
  ## Normality with QQ Plot
  plot(model1, which = 2)
  ## Homoscedasticity
  plot(model1, which = 1)
  ## Independence
  plot(model1$residuals, type = "b", ylab = "Residuals")
  abline(h=0, lty="dashed")
}

# Display the ANOVA Report in Console
{
  summary.aov(model1) # Preferred method
  summary(model1) # Works only for oneway anova
  anova(model1)
}

## To suppress the stars report
{
  print(anova(model1), signif.stars = F)
  # OR
  options(show.signif.stars = FALSE)
}

# Varietal Effects
{
  dummy.coef(model1)
  ## R by default uses the constraint that the first level
  ## (alphabetical) will be set to zero.

  #Using our constraint (sum to zero)
  ## Check your contrasts settings
  options("contrasts")
  ## Alter your contrasts settings
  options(contrasts = c("contr.sum","contr.poly"))
  model2 <- aov(Surplus ~ Varietal, data = honey)
  dummy.coef(model2)
}

# Make a nice ANOVA Table for a report
{
  options(knitr.kable.NA= "") # Keeps NA from appearing in the table
  knitr::kable(anova(model1),
          digits = 3,
          align = rep('c',5),
          col.names = c("DF", "Sum Sq.", "Mean Sq.", "F Ratio", "p-value"),
          caption = "Oneway ANOVA Table for Honey Data"
  ) %>% #Piping is from a tidyverse call (dplyr)
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position"
  )
}

# Effect Sizes--Using sjstats
{
  eta2 <- sjstats::eta_sq(model2)
  w2 <- sjstats::omega_sq(model2)
  eps2 <- sjstats::epsilon_sq(model2)
}

# Create a Modern ANOVA Table for your report--easy method
{
  options(knitr.kable.NA= "")
  knitr::kable(
    parameters::model_parameters(
      model1, omega_squared = "raw",
      eta_squared = "raw", epsilon_squared = TRUE),
  digits = 3,
  col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                "Omega Sq.", "Eta Sq.", "Epslion Sq."), # Nice Column Names
  caption = "ANOVA Table for Surplus Honey"
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")
}
