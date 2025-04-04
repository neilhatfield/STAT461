checkSetup <- function(){
  ## Check R Version ----
  currentVersion <- "4.3.1"
  errorMessages <- c()
  if (as.numeric(version$major) < 4) {
    stop("Your R version is too far out of date. Update before continuing.")
  } else if (as.numeric(version$minor) < 3) {
    stop("You need to update your R to at least 4.3.0")
  } else if (paste(version$major, version$minor, sep = ".") < currentVersion) {
    warning("You should update your R when possible.")
  } else {
    print("R Version is current.")
  }

  ## Define package lists ----
  corePackages <- c("tidyverse", "devtools", "knitr", "car", "psych",
                   "parameters", "rcompanion", "DescTools", "dunn.test",
                    "multcompView", "emmeans", "rstatix", "lme4",
                    "nlme", "tinytex", "rmarkdown", "sjstats", "effectsize")
  ### kableExtra is temporally a separate install 
  optionalPackages <- c("lattice", "perm", "boot", "coin", "pwr", "WebPower",
                        "openxlsx", "NSM3", "agricolae")
  

  ## Package installation ----
  pkgInstall <- menu(
    choices = c("Install Everything (Recommended)", "Install Only Required", "Skip Package Install"),
    title = "What packages do you want installed?"
  )
  if (pkgInstall == 1) {
    pkgList <- c(corePackages, optionalPackages)
  } else if (pkgInstall == 2) {
    pkgList <- corePackages
  } else {
    pkgList <- NA
  }

  if (pkgInstall == 1 || pkgInstall == 2) {
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
      devtools::install_version("hasseDiagram", "0.2.0")
    }
    
    ### Install last working version of kableExtra ----
    #### kableExtra v 1.4.0 is breaking on scale_down
    devtools::install_version(package = "kableExtra", version = "1.3.4")

    print("Finished installing packages")
  }

  if (pkgInstall >= 0) {
    ## Tex Installation
    texInstall <- menu(
      choices = c("Yes", "No"),
      title = "Do you want to install Tex/LaTeX? (Useful for making nice PDFs)"
    )

    if (texInstall == 1) {
      print("Installing TinyTeX...")
      tinytex::install_tinytex()
      print("Finished installing TinyTex")
    }
  }

  print("All finished. Your computer is ready for Stat 461!")
}
