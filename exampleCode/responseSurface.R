# Response Surface Example
# Let's use an Unusalness Threshold of 0.07
# Note: we'll be relaxed about Type I Errors at the moment.

# Load Packages
packages <- c("tidyverse", "rsm", "ggplot2")
lapply(packages, library, character.only = TRUE)

# Study 1-Inital Exploration of Baking Time and Temp
# Central Composite Design: 2^2 Factorial + m center points

## Create Data Frame
baking1 <- data.frame(
  time = c(33,37,33,37,35,35,35),
  temp = c(340,340,360,360,350,350,350),
  appeal = c(3.89, 6.36, 7.65, 6.79, 8.36, 7.63, 8.12)
)

## Code Data values
coded1 <- rsm::coded.data(baking1)
names(coded1) <- c("cTime", "cTemp", "appeal")

## Will a First Order Model Work?
rsm::varfcn(coded1, ~ rsm::FO(cTime,cTemp),
            contour = TRUE,
            main = "Variance Function for 1st Experiment")

## Will a Second Order Model Work?
rsm::varfcn(coded1, ~ rsm::SO(cTime,cTemp),
            contour = TRUE,
            main = "Variance Function for 1st Experiment")
### No, notice the error message.

## Fit the First Order Model
bakeRS1 <- rsm::rsm(appeal ~ rsm::FO(cTime, cTemp), data = coded1)

## Perspective Plot for First Order Model
persp(bakeRS1, form = ~ cTime + cTemp)

## Contour Plot
contour(bakeRS1, form = cTemp ~ cTime)

## Look out raw output of the model
summary(bakeRS1)

# Study 2-Second Experiment
# Central Composite Design: 2^2 Factorial + m center points + Axial Points

## Read in the Coded Data File
baking2 <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/bake2.dat",
  header = TRUE
)
baking2$block <- as.factor(baking2$block)
### Note: Block 1 values are identical to the original data frame

## We need to say that the data are already coded.
coded2 <- rsm::as.coded.data(data = baking2,
                             time ~ (oTime - 35) / 2,
                             temperature ~ (oTemperature - 350) / 20,
                             block = "block")

## Will a Second Order Model Work?
rsm::varfcn(coded2, ~ rsm::SO(time, temperature),
            contour = TRUE,
            main = "Variance Function for 2nd Experiment")

## Fit Second Order Model
bakeRS2 <- rsm::rsm(
  appeal ~ block + SO(time, temperature),
  data = coded2)

## Perspective Plot
persp(bakeRS2, form = ~ time + temperature)

## Contour Plot
contour(bakeRS2, form = temperature ~ time)

## Look out raw output of the model
summary(bakeRS2)
