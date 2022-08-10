# STAT461

Repo for Stat 461-ANOVA

I will use this repository to store and share data files and other resources for my sections of Stat 461-ANOVA at Penn State.

## Key Elements

There are several key elements to this repo:

+ Homework Template File
+ activityHandouts
+ dataFiles
+ demoFiles
+ rScripts
+ exampleCode (archive)
+ learnrTutorials (under development)
+ writingReports (under development)
+ Misc (-ellaneous)

## Homework Template File

I have included a template file you may use for assignments if you are using R Markdown. The template is based upon one developed by [Alex Hayes](https://github.com/alexpghayes/rmarkdown_homework_template).

## Activity Handouts

This directory contains various RMD files for different activities used throughout the course.

### Data Files

This sub-directory contains various data files (mostly *.dat) that I have used in STAT 461 either as in-class examples, homework assignments, or projects.

### Demo Files

This sub-directory will be the home of various R Markdown files and future files that help students learn R and develop fluency.

## R Scripts

This sub-directory contains R files which define multiple helpful functions for Stat 461.

### Check Setup

The `checkSetup` function will check your R setup for three things: 1) do you have a current version of R (i.e., 4.2.1+) do you have the necessary packages installed (if not, it will install them), 3) will ask you if you want to install TinyTeX (and install it if you do).

To use this function copy and paste the following code in your Console and hit Return:
```
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/checkSetup.R")
checkSetup()
```

### ANOVA Tools

The ANOVATools.R file contains a set of functions that I've written to help individuals perform several common tasks related to ANOVA. There are public and private functions in this file.

#### Public Functions

As of 3/20/2022, there are nine public functions: `anova.PostHoc`, `anovaFixer`, `block.RelEff`, `hodgesLehmann`, `kw.PostHoc`, `probSup`, `pvalRound`, `sphericityPlot`, and `dscfTest`.

##### anova.PostHoc

The `anova.PostHoc` function takes three arguments: `aov.obj`-required, `response`-optional, and `mainEffect`-optional. The `aov.obj` should be the output of the function `aov` in R. The `response` and `mainEffect` arguments take a string value to identify the response of the model as well as the main effect of interest for post hoc analysis. These two optional arguments are useful for conducting Post Hoc analysis for Oneway + Blocking ANOVA as well as exploring Main Effects for a Factorial ANOVA.

The `anova.PostHoc` returns a data frame of effect sizes that can be formatted by another function (e.g., `knitr::kable`) consisting of the following columns: Pair , Cohen's *d*, Hedge's *g*, and the Probability of Superiority.

Future plans include adding on handling of interactions for Factorial ANOVAs.

##### anovaFixer

The `anovaFixer` function will return a data frame for use in a `kable` call that adjusted denominators for mixed effect models. I'm currently pondering whether to retire this function.

##### block.RelEff

The `block.RelEff` function takes three arguments, all required: `aov.obj`, `blockName`, `trtName`. The `aov.obj` should be the output of the function `aov` in R using a formula that contains a block term. The `blockName` should be a string identifying the column name of the block, and `trtName` should be a string identifying the column name for the treatment.

The `block.RelEff` will return a string giving the relative efficiency of the randomized complete block design used when compared to a complete randomized design. A relative efficiency of *x* means that we would need *x* times as many measurement units per treatment level as what we used in the RCB design to produce the same level of information.

##### hodgesLehmann

The `hodgesLehmann` function takes two vectors of values, *x* and *y*, and calculates the value of Hodges-Lehman ![equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Cwidehat%7B%5CDelta%7D). <!--(\widehat{\Delta}\)-->

##### kw.PostHoc

The `kw.PostHoc` function takes two arguments, both required: `x`, and `g`. The `x` argument should be the response vector and `g` a vector of group membership (i.e., the factor). 

The `kw.PostHoc` function returns a data frame of effect sizes that can be formatted by another function (e.g., `knitr::kable`) consisting of the following columns: Pair , Hodges-Lehmann ![equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Cwidehat%7B%5CDelta%7D), <!--\(\widehat{\Delta}\)--> and the Probability of Superiority.

##### probSup

The `probSup` function takes a Cohen's *d* value as the input and returns the Probability of Superiority, via
![equation](https://latex.codecogs.com/gif.latex?PS%28d%29%3D1-C_%7B%5Cmathcal%7BN%7D%7D%5Cleft%28%5Cfrac%7B-d%7D%7B%5Csqrt%5B2%5D%7B2%7D%7D%5Cbigg%7C0%2C1%5Cright%29)
<!--\[PS(d)=1-C_{\mathcal{N}}\left(\frac{-d}{\sqrt[2]{2}}\bigg|0,1\right)\]-->

##### pvalRound

The `pvalRound` function looks at a value (particularly *p*-values) and if the value is less than 0.0001 will return the character string "< 0.0001", otherwise will return the value rounded to four decimal places. This is to help limit instances were rounding make *p*-values look like zero.

##### sphericityPlot (under development)

The `sphericityPlot` provides a scatter plot of the pairwise differences of the treatments for each subject in a Within Subjects Repeated Measures design.

##### dscfTest

The `dscfTest` a function of two vectors, both required: `response` containing the response values and `factor` containing the levels of the factor. The two vectors should be in the same order. This function is a wrapper for `NSM3::pSDCFlig` function for doing the Steel-Dwass-Critchlow-Fligner post hoc test in a One-way nonparametric setting. This will return a data frame ready for `kable` consisting of the comparisons and their (adjusted) *p*-values.

#### Private Functions

As of 3/30/2021, there is one private function which acts as a helper function: .strsplitN.

##### .strsplitN

The `.strsplitN` function splits strings generated by `dunn.test::dunn.test` on the "-" and returns *N*th word.

### Shadowgram (under development)

The `shadowgram` function is under development. The goal is the bring the shadowgram from JMP to R via `ggplot2`. While the current code functions, there is still more work to be done to fine tune the function.

You can access this function through the following means

```
source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/shadowgram.R")

# Example usage
shadowgram(
  dataVec = oreoData$Filling.Mass,
  label = "Filling mass (g)",
  layers = 60,
  aStep = 4,
  color = "blue"
)

```

## Example Code (archive)

This sub-directory is acting as an archive of individual R files that served as example code for Spring 2020.

## learnrTutorials

This sub-directory is where I'm working on creating some learnr based tutorials for STAT 461. They are in rather incomplete states.

## writingReports

This is a new (as of Spring 2022) directory where I'm placing materials related to helping students write reports of their results in the ANOVA context.
