# Nested Repeated Measures Example
# Guinea Pig Weights and Vitamin E

# Packages
packages <- c("tidyverse", "hasseDiagram", "car",
              "ggplot2", "afex", "emmeans")
lapply(packages, library, character.only = TRUE)

# Load Helper Functions
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Set Global Options
options("contrasts" = c("contr.sum","contr.poly"))

# Hasse Diagram
gpLab <- c("1 Grand Mean 1", "3 Vit. E 2", "6 Time Point 5",
           "15 (pigs) 12", "18 Vit. E X Time 10",
           "90 (pigs X Time) 60")
gpMat <- matrix(data = F, nrow = 6, ncol = 6)
gpMat[1, c(2:6)] = gpMat[2, c(4,5,6)] = T
gpMat[3, c(5, 6)] = gpMat[c(4:5), 6] = T
hasseDiagram::hasse(gpMat, gpLab)

# Data File
guineaPigs <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/guineaPigs.dat",
  header = TRUE, sep = ","
)
guineaPigs$Subject <- as.factor(guineaPigs$Subject)

# Data are in Wide Format, we need will need Long Format for some calls
pigsL <- tidyr::pivot_longer(guineaPigs,
                  cols = dplyr::starts_with("Week"),
                  names_to = "Time",
                  names_ptypes = list("Time" = factor()),
                  values_to = "weight")

# Explore the data-On Your Own
## Data Visualizations
## Descriptive Statistics

# Fit the Nested Repeated Measures Model
model1 <- aov(weight ~ trt*Time + Error(Subject/Time), data = pigsL)

# Check assumptions-On Your Own

# Omnibus Test
## Using the aov call, Raw Output
summary(model1)

### Professional looking is On Your Own

## Multivariate approach--Uses the original data frame
### Isolate the response values
weights <- as.matrix(guineaPigs[ , 3:8])

### Fit multivariate model--Notice the formula
model2 <- lm(weights ~ 1 + trt, data = guineaPigs )

### Define the Repeated Factor
weeks <- factor(c("Week1","Week3","Week4","Week5","Week6","Week7"))

### Create the table object
tabPigs <- car::Anova(model2, idata = data.frame(weeks),
                      idesign = ~weeks, type = "III")

### Raw Output
outRM <- summary(tabPigs, multivariate=FALSE)

### Professional Tables are On Your Own

## Omnibus Method 3--Using the Afex package
## This blends the two methods together
model3 <- afex::aov_car(weight ~ trt*Time + Error(Subject/Time),
                        data = pigsL)

## Raw Output
summary(model3)

# Post Hoc Analysis

## No Interaction
phMeansTime <- emmeans::emmeans(model3, ~Time,
                                adjust = "tukey",
                                level = 0.9)
phMeansTrt <- emmeans::emmeans(model3, ~trt,
                               adjust = "tukey",
                               level = 0.9)
### Raw Output
phMeansTime
phMeansTrt

## Interaction
phMeansInt <- emmeans::emmeans(model3, ~Time|trt)
phMeansInt

##Pairwise
pairs <- emmeans::emmeans(model3, pairwise ~ Time|trt,
                 adjust = "tukey",
                 level = 0.9)
pairs$emmeans
pairs$contrasts

pairs2 <- emmeans::emmeans(model3, pairwise ~ trt|Time,
                           adjust = "tukey",
                           level = 0.9)

## Growth Curves
ggplot2::ggplot(data = pigsL,
                mapping = ggplot2::aes(
                  y = weight,
                  x = Time,
                  group = Subject,
                  color = trt )) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(size = 1) +
  ggplot2::theme_bw() +
  xlab("Time Point") +
  ylab("Weight (grams)") +
  labs(color = "Vitmain E Dosage")
### Hard to Read, try spliting by treatment instead of color
ggplot2::ggplot(data = pigsL,
                mapping = ggplot2::aes(
                  y = weight,
                  x = Time,
                  group = Subject)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(size = 1) +
  ggplot2::facet_grid(trt ~ .) +
  ggplot2::theme_bw() +
  xlab("Time Point") +
  ylab("Weight (grams)")