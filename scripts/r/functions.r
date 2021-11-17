################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] functions.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 8 March 2020
##
################################################################################

## -----------------------------------------------------------------------------
## general math
## -----------------------------------------------------------------------------

## NB: using log transforms to make more numerically stable

## inverse logit
##
##                  exp(x)           1
## logit^-1(x) = ------------ = ------------- = exp(-log(1 + exp(-x)))
##                exp(x) + 1     1 + exp(-x)
##
inv_logit <- function(x) { exp(-log(1 + exp(-x))) }

## multiply probabilities
##
## p(x)p(y) = exp(log(x) + log(y))
##
mult_probs <- function(x, y) { exp(log(x) + log(y)) }

## MRP: weighting equation
##
##         \sum \theta_i * n_i
## theta = ------------------- = SUM exp(log(theta_i) + log(n_i) - log(N))
##                  N
##
ps_weight <- function(x, n) { colSums( x * n / sum(n)) }

## MRP: weighting equation 2
##
##         \sum \theta1_2_i * n_i
## theta = ----------------------- = SUM exp(log(theta1_2_i) + log(n_i) - )
##            n_i * \theta1_i
##
ps_weight_2 <- function(x, y, n) {
    colSums(exp(log(x) + log(y)) * n / colSums(y * n))
}

## -----------------------------------------------------------------------------
## reading in data
## -----------------------------------------------------------------------------

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
