################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] make_mrp_data.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 5 November 2019
##
################################################################################

## libraries
libs <- c("haven", "tidyverse", "rstan")
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
helios_clean <- "analysis_data.dta"
sl_dat_clean <- "second_level_df.rds"

## -----------------------------------------------------------------------------
## FUNCTIONS
## -----------------------------------------------------------------------------

make_stan_data <- function(df_fl,          # 1st-level data frame
                           sl_fl,          # 2nd-level data frame
                           outcome,        # outcome of interest (e.g. "q3_1")
                           school_index,   # school index column name
                           design_matrix){ # design matrix

    message("     - ", outcome)
    ## make first data set, which contains successes/trials for each
    ## group; rather than leave each observation and use 0/1 outcomes,
    ## we can take advantage of the fact that a bernoulli distribution
    ## is just a special case of the binomial distribution in which
    ## there's only one trial; by collapsing to clicks/views ---
    ## which are a sufficient statistic --- the data set we send to
    ## Stan is smaller

    ## drop missing outcomes
    fl_tmp <- df_fl %>%
        select(one_of(outcome, school_index), ends_with("_cat"))  %>%
        drop_na({{ outcome }})

    ## get counts (for binomial set up)
    fl_compress <- fl_tmp %>%
        group_by(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
        summarise(clicks = sum(!!sym(outcome)),
                  views = n(),
                  .groups = "drop") %>%
        left_join(df_sl, by = "school_i")

    ## get tag for cells in universal design matrix
    in_dm <- fl_compress %>%
        right_join(design_matrix,
                   by = c("school_i", "gen_cat", "rac_cat", "age_cat",
                          "gpa_cat", "hrs_cat")) %>%
        mutate(in_dmat = ifelse(is.na(views), 0, 1)) %>%
        arrange(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
        pull(in_dmat)

    ## get data parts to send to Stan
    N <- nrow(fl_compress)
    J_sch <- fl_compress %>% distinct(school_i) %>% nrow
    J_gen <- fl_compress %>% distinct(gen_cat) %>% nrow
    J_rac <- fl_compress %>% distinct(rac_cat) %>% nrow
    J_age <- fl_compress %>% distinct(age_cat) %>% nrow
    J_gpa <- fl_compress %>% distinct(gpa_cat) %>% nrow
    J_hrs <- fl_compress %>% distinct(hrs_cat) %>% nrow
    views <- select(fl_compress, views) %>% pull %>% as.vector
    clicks <- select(fl_compress, clicks) %>% pull %>% as.vector
    sch <- fl_compress %>% pull(school_i) %>% as.vector
    gen <- fl_compress %>% pull(gen_cat) %>% as.vector
    rac <- fl_compress %>% pull(rac_cat) %>% as.vector
    age <- fl_compress %>% pull(age_cat) %>% as.vector
    gpa <- fl_compress %>% pull(gpa_cat) %>% as.vector
    hrs <- fl_compress %>% pull(hrs_cat) %>% as.vector
    z <- fl_compress %>%
        select(school_i,
               comp150_std,
               npist2_std,
               wk_wage_std,
               unem_rate_std) %>%
        distinct %>%
        select(-school_i) %>%
        as.matrix
    M <- nrow(z)
    L <- ncol(z)

    ## save as data dump that Stan can read
    stan_rdump(c("N","M","L","J_sch","J_gen","J_rac","J_age","J_gpa","J_hrs",
                 "views","clicks","sch","gen","rac","age","gpa","hrs","z","in_dm"),
               file = file.path(dat_dir,
                                paste0("stan_dat_", outcome, ".Rdump")))
}

## -----------------------------------------------------------------------------
## READ / MUNGE SURVEY DATA
## -----------------------------------------------------------------------------

## first-level: cleaned Helios survey data
df_fl <- read_dta(file.path(dat_dir, helios_clean))

## second-level variables
df_sl <- readRDS(file.path(dat_dir, sl_dat_clean))

## poststratification matrix (to make design matrix)
design_matrix <- read_dta(file.path(dat_dir, "poststrat_counts.dta")) %>%
    arrange(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
    select(-count)

## -----------------------------------------------------------------------------
## SET UP STAN DATA FOR OUTCOMES OF INTEREST
## -----------------------------------------------------------------------------

## vector of outcomes: "q3_1", "q3_2", ...
outcomes <- c(paste0("q3_", 1:5),
              "q4",
              paste0("q5_", 1:5),
              paste0("q6_", 1:6),
              paste0("q7_", 1:5),
              paste0("q8_", 1:5),
              paste0("q9_", 1:5),
              "q10",
              paste0("q11_", 1:5),
              paste0("q12_", 1:5))

## walk through each, making a Stan *.Rdump file for each
message("  > Creating Stan data for:")
walk(outcomes, ~ make_stan_data(df_fl,          # 1st-level data frame
                                df_sl,          # 2nd-level data frame
                                .x,             # question name
                                school_index,   # school index variable name)
                                design_matrix)) # design matrix from macros.R

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
