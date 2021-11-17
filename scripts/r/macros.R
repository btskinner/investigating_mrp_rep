################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] macros.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 6 February 2020
##
################################################################################

## -----------------------------------------------
## crosswalk
## -----------------------------------------------

school_cw <- tibble(school = c("BC","HCC","MDC","PB","VC"),
                    school_i = 1:5,
                    unitid = c(132709,134495,135717,136358,138187))

## -----------------------------------------------
## general settings
## -----------------------------------------------

## school index variable name
school_index <- "school_i"

## read in each Stan data file
get_rstan_data_list <- function(path,                # path to stan_dat_q* files
                                pattern){            # e.g., "stan_dat_q*"

    ## pull list of stan_dat_q* data file names w/ path
    files <- list.files(path, pattern, full.names = TRUE)
    ## read files into list
    out <- map(files,
               ~ read_rdump(.x)) %>%
        setNames(gsub("^.+stan_dat_(q.+)\\.Rdump", "\\1", files))
}

################################################################################
## END SCRIPT
################################################################################
