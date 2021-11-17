################################################################################
##
## [ PROJ ] Helios survey with MRP
## [ FILE ] get_packages.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 14 January 2020
##
################################################################################

## -----------------
## CRAN
## -----------------

## required packages
recpkgs <- c("tidyverse",
             "devtools",
             "rstan",
             "xtable")

## compare against already installed
misspkgs <- recpkgs[!(recpkgs %in% installed.packages()[,"Package"])]

## install those that are missing
if (length(misspkgs)) {
    install.packages(misspkgs)
} else {
    message("- All required CRAN packages already installed!")
}

## -----------------
## GitHub
## -----------------

## required package
recpkgs <- c("cmdstanr")

## compare against already installed
misspkgs <- recpkgs[!(recpkgs %in% installed.packages()[,"Package"])]

## get github packages
if (length(misspkgs)) {
    devtools::install_github("stan-dev/cmdstanr")
    ## check for cmdstan installation
    cmdstan_exists <- tryCatch(
    {
        cmdstanr::install_cmdstan()
    }, warning = function(w) return(TRUE)
    )
    if (!cmdstan_exits) {
        message("- Installing cmdstan to default location")
        cmdstanr::install_cmdstan(cores = parallel::detectCores())
    }
} else {
    message("- All required GitHub packages already installed!")
}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
