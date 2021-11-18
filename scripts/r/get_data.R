################################################################################
##
## [ PROJ ] Helios survey with MRP
## [ FILE ] get_data.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 6 February 2020
##
################################################################################

## libraries
libs <- c("tidyverse", "readxl")
invisible(suppressMessages(sapply(libs, require, character.only = TRUE)))

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data", "clean")
est_dir <- file.path(root, "data", "estimates")
ext_dir <- file.path(root, "data", "external")
sta_dir <- file.path(root, "scripts", "stan")
scr_dir <- file.path(root, "scripts", "r")

## macros
source(file.path(scr_dir, "macros.R"))

## data
bls_laucnty <- "laucnty18.txt"
bls_wages <- "2018_annual_by_area.zip"
ipeds_files <- c("HD2018", "EF2018D", "SFA1718", "GR2018",
                 "HD2019", "EFFY2019", "SFA1819", "GR2019",
                 "C2019_C")
sl_dat_clean <- "second_level_df.rds"

## -----------------------------------------------------------------------------
## GET EXTERNAL DATA
## -----------------------------------------------------------------------------

message("- Munging external data")
## -----------------------------------------------
## Bureau of labor statistics: unemployment
## -----------------------------------------------

## BLS raw file
bls <- file.path(ext_dir, bls_laucnty)

## download raw file if not already downloaded
if (!file.exists(bls)) {
    message("  - Downloading BLS data for unemployment rates...")
    ## BLS url
    url <- file.path("https://www.bls.gov/lau", bls_laucnty)
    ## download
    download.file(url, bls)
}

## munge data; the file has extra header information, which needs to
## be skipped; also, we only need Florida unemployment data, so we can
## drop the last two lines, which aren't part of the data anyway. X2
## and X3 are columns with state and county FIPS, respectively, and X9
## is the unemployment rate.

unem <- read_fwf(bls, fwf_empty(bls, skip = 7), skip = 7, n_max = 3218) %>%
    filter(X2 == "12") %>%
    mutate(fips = as.numeric(paste0(X2, X3)),
           unem_rate = as.numeric(X9),
           unem_rate_std = scale(unem_rate)) %>%
    select(fips, unem_rate, unem_rate_std)

## -----------------------------------------------
## IPEDS
## -----------------------------------------------

## download if file is missing; unzip
for (i in file.path(ext_dir, paste0(ipeds_files, ".zip"))) {
    if (!file.exists(i)) {
        message("  - Downloading IPEDS data...")
        base <- "https://nces.ed.gov/ipeds/datacenter/data/"
        walk(ipeds_files,
             ~ download.file(paste0(base, .x, ".zip"),
                             file.path(ext_dir, paste0(.x, ".zip")),
                             quiet = TRUE))
    }
}

## HD
## unitid   := Unique identification number of the institution
## countycd := Fips County code
## sector   := Sector of institution
## ccbasic  := Carnegie Classification 2005/2010: Basic
## locale   := Degree of urbanization (Urban-centric locale)
hd <- read_csv(file.path(ext_dir, "HD2018.zip")) %>%
    rename_all(tolower) %>%
    filter(unitid %in% pull(school_cw, unitid)) %>%
    select(unitid, fips = countycd)

## SFA
## unitid  := Unique identification number of the institution
## npist2  := Average net price-students awarded grant or scholarship aid,
##            2017-2018
sf <- read_csv(file.path(ext_dir, "SFA1718.zip")) %>%
    rename_all(tolower) %>%
    filter(unitid %in% pull(school_cw, unitid)) %>%
    mutate(npist2_std = scale(npist2)) %>%
    select(unitid, npist2, npist2_std)

## GR
## unitid  := Unique identification number of the institution
##
## This is a long data file, so we're getting the rows with the total
## number of students in the revised cohort (line == 10) and the
## number of students who graduated within 150% normal time (line ==
## 29A), reshaping, then calculating the percentage.
gr <- read_csv(file.path(ext_dir, "GR2018.zip")) %>%
    rename_all(tolower) %>%
    filter(unitid %in% pull(school_cw, unitid), line %in% c("10","29A")) %>%
    select(unitid, chrtstat, grtotlt) %>%
    mutate(type = ifelse(chrtstat == 10, "tot", "comp")) %>%
    select(-chrtstat) %>%
    pivot_wider(names_from = "type",
                values_from = "grtotlt") %>%
    mutate(comp150 = round(comp / tot * 100, 2),
           comp150_std = scale(comp150)) %>%
    select(unitid, comp150, comp150_std)

## -----------------------------------------------
## Bureau of labor statistics: wages
## -----------------------------------------------

## BLS raw file
bls <- file.path(ext_dir, bls_wages)

## download raw file if not already downloaded
if (!file.exists(bls)) {
    message("  - Downloading BLS data for wages...")
    ## BLS url
    url <- file.path("https://www.bls.gov/cew/data/files/2018/csv/", bls_wages)
    ## download
    download.file(url, bls)
    ## unzip
    unzip(bls, exdir = ext_dir)
}

## pull school fips from HD file
sch_fips <- hd %>% pull(fips)

## read in each county-specific file
wage <- map(sch_fips,
            ~ read_csv(list.files(file.path(ext_dir,
                                            "2018.annual.by_area"),
                                  pattern = as.character(.x),
                                  full.names = TRUE)) %>%
                ## give average across all industries
                filter(agglvl_code == 70) %>%
                ## just keep average weekly wage
                select(fips = area_fips, wk_wage = annual_avg_wkly_wage)) %>%
    ## bind together
    bind_rows %>%
    ## standardize wage
    mutate(wk_wage_std = scale(wk_wage))

## -------------------------------------------------------------------------
## JOIN EXTERNAL DATA AND SAVE
## -------------------------------------------------------------------------

## join IPEDS together
df <- Reduce(left_join, list(hd, sf, gr)) %>%
  left_join(school_cw, by = "unitid") %>%
  select(-school, -unitid)

## join in unemployment rate
df <- df %>%
  left_join(unem, by = "fips") %>%
  ## join in wages
  left_join(wage, by = "fips") %>%
  ## subset to variables of interest
  select(school_i, comp150_std, npist2_std, wk_wage_std, unem_rate_std)

## save
saveRDS(df, file.path(dat_dir, sl_dat_clean))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
