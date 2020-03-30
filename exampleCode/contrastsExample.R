library(DescTools)
options(show.signif.stars = FALSE)
ratLiver <- read.table(
  file = "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/ratLiver.dat",
  header = TRUE, sep = ",")
ratLiver$diet <- as.factor(ratLiver$diet)
options("contrasts" = c("contr.sum","contr.poly"))
ratModelA <- aov(liverWeight ~ diet, data = ratLiver)

# Get Cell Means
model.tables(ratModelA, "means", digits = 2)

# Check the ordering of your factor
levels(ratLiver$diet)

# Contrast
c1 <- c(1/3, 1/3, 1/3, -1)
c2 <- c(1/2, -1/2, 1/2, -1/2)
contrasts(ratLiver$diet) <- cbind(c1, c2)
ratModelC <- aov(liverWeight ~ diet, data = ratLiver)
# Does not show the Contrast
anova(ratModelC)

# Shows the Contrast--No Type I Error Control
summary.aov(ratModelC, split=list(
  diet = list("1,2,3 vs. 4" = 1,
              "1,3 vs. 2,4" = 2)
))

# Scheffe Control
DescTools::ScheffeTest(ratModelC,
                       contrasts = cbind(c1, c2),
                       conf.level = 0.9)

