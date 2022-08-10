checkSetup <- function(){
  ## Check R Version ----
  currentVersion <- "4.2.1"
  errorMessages <- c()
  if (as.numeric(version$major) < 4) {
    stop("Your R version is too far out of date. Update before continuing.")
  } else if (paste(version$major, version$minor, sep = ".") != currentVersion) {
    warning("You should update your R when possible.")
  } else {
    print("R Version is current.")
  }
  
  ## Define package lists ----
  corePackages <- c("tidyverse", "knitr", "kableExtra", "car", "psych",
                   "parameters", "rcompanion", "DescTools", "dunn.test",
                    "multcompView", "emmeans", "rstatix", "lme4", 
                    "nlme", "tinytex", "rmarkdown")
  optionalPackages <- c("lattice", "perm", "boot", "coin", "pwr", "WebPower", "openxlsx")

  ## Package installation ----
  pkgInstall <- menu(
    choices = c("Install Everything", "Install Only Required", "Skip Package Install"),
    title = "What packages do you want installed?"
  )
  if (pkgInstall == 1) {
    pkgList <- c(corePackages, optionalPackages)
  } else if (pkgInstall == 2) {
    pkgList <- corePackages
  } else if {
    pkgList <- NA
  }

  if (!is.na(pkgList)) {
    ## Check installed packages ----
    print("Checking for installed packages...")
    newPackages <- pkgList[!(pkgList %in% installed.packages()[,"Package"])]
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
  }

  if (pkgInstall > 0) {
    ## Tex Installation
    texInstall <- menu(
      choices = c("Yes", "No"),
      title = "Do you want to install Tex/LaTeX ?"
    )

    if (texInstall == 1) {
      print("Installing TinyTeX...")
      tinytex::install_tinytex()
      print("Finished installing TinyTex")
    }
  }

  print("All finished. Your computer is ready for Stat 461!")
}
