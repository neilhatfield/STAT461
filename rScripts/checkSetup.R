checkSetup <- function(){

  ## Check R Version ----
  currentVersion <- "4.0.3"
  errorMessages <- c()
  if (as.numeric(version$major) < 4) {
    stop("Your R version is too far out of date. Update before continuing.")
  } else if (paste(version$major, version$minor, sep = ".") != currentVersion) {
    warning("You should update to version 4.0.3 as soon as possible.")
  } else {
    print("R Version is good.")
  }

  ## Check installed packages ----
  print("Checking for installed packages...")
  reqPackages <- c("tidyverse", "knitr", "kableExtra", "car", "sjstats", "psych",
                   "parameters", "lattice", "lme4", "DescTools", "afex", "emmeans",
                   "rstatix", "rsm", "coin", "rcompanion", "boot", "lmboot",
                   "multicool", "dunn.test", "tinytex", "rmarkdown", "perm"
                   )
  newPackages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]
  if (length(newPackages)) {install.packages(newPackages)}

  ### Check for Bioconductor ----
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  if (!("Rgraphviz" %in% installed.packages()[, "Package"])) {
    BiocManager::install("Rgraphviz")
  }

  ### Install hasseDiagrams package ----
  if (!("hasseDiagram" %in% installed.packages()[, "Package"])) {
    install.packages("hasseDiagram")
  }

  print("Finished installing packages")

  ## Tex Installation
  texInstall <- "prompt"
  while (!(texInstall %in% c("yes", "y", "no", "n"))) {
    texInstall <- readline(prompt = "Do you want to install TeX/LaTex? Yes/No: ")
    texInstall <- tolower(texInstall)
  }

  if (texInstall == "yes" || texInstall == "y") {
    print("Installing TinyTeX...")
    tinytex::install_tinytex()
    print("Finished installing TinyTex")
  }

  print("All finished. Your computer is ready for Stat 461!")
}