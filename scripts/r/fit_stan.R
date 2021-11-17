################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] fit_stan.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 14 November 2019
##
################################################################################

## libraries
libs <- c("cmdstanr", "tidyverse", "rstan")
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

## functions
source(file.path(scr_dir, "functions.R"))

## -------------------------------------
## Stan-specific settings
## -------------------------------------

## stan file
stan_file <- file.path(sta_dir, "mrp.stan")

## stan sampler settings
stan_seed <- 19806
stan_adapt_delta <- .99
stan_max_depth <- 15L
stan_num_cores <- parallel::detectCores()
stan_num_chains <- 4
stan_num_threads <- 1
stan_num_warmup <- 1000L
stan_num_samples <- 1000L

## -----------------------------------------------------------------------------
## READ DATA FOR MODELS
## -----------------------------------------------------------------------------

## we want to run MRP for each outcome; since each outcome has its
## own data set, we just read in a list of the stan_dat_q*.Rdump files;
## for each data file, we read it into memory, center the 1st and
## 2nd-level covariates, and add the file name to the list name (to
## make sure that everything aligns later)
data_list <- get_rstan_data_list(path = dat_dir,
                                 pattern = "stan_dat_q*")

## -----------------------------------------------------------------------------
## COMPILE STAN MODEL
## -----------------------------------------------------------------------------

## compile Stan model from mrp.stan script; it's small so it's not a
## big deal to recompile each run, but we don't have to do so
mod <- cmdstan_model(stan_file,
                     compile = TRUE,
                     cpp_options = list(stan_threads = TRUE))

## -----------------------------------------------------------------------------
## COMPUTE POSTERIOR FOR EACH OUTCOME IN LIST
## -----------------------------------------------------------------------------

## mod$sample() is the main command; since we have so many models, we
## just run through each one at a time, storing the resulting fit
## object in a list (NB: it should have the name of the data list
## object we set above, so that makes life easier)

## OPTIONS:
## seed := value doesn't matter, but for replication, we choose one
## adapt_delta (default: .80) := sets stepsize for HMC
## num_cores := use maximum cores to speed computation
## num_chains := want a few to check convergence
fit_list <- map(data_list,
                ~ mod$sample(.x,
                             seed = stan_seed,
                             adapt_delta = stan_adapt_delta,
                             max_treedepth = stan_max_depth,
                             iter_warmup = stan_num_warmup,
                             iter_sampling = stan_num_samples,
                             parallel_chains = stan_num_cores,
                             chains = stan_num_chains,
                             threads_per_chain = stan_num_threads))

## -----------------------------------------------------------------------------
## EXTRACT INFORMATION
## -----------------------------------------------------------------------------

## we "read" the output from each model output object and extract
## everything; we should end up with a list of lists, with the
## parameters being in vectors or matrices
full_fit_list <- map(fit_list,
                     ~ read_stan_csv(.x$output_files()))

## -----------------------------------------------------------------------------
## SAVE POSTERIORS LIST
## -----------------------------------------------------------------------------

saveRDS(full_fit_list, file.path(est_dir, "full_fit_list.rds"))

## -----------------------------------------------------------------------------
## DIAGNOSTICS
## -----------------------------------------------------------------------------

## -------------------------------------
## calculate divergences
## -------------------------------------

diverg <- map(full_fit_list,
              ~ get_num_divergent(.x)) %>%
    bind_rows %>%
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "num_divergent") %>%
    mutate(pct_divergent = num_divergent /
               (stan_num_samples * stan_num_chains) * 100)

## -------------------------------------
## combine all values into on data frame
## -------------------------------------

diag <- diverg

## -------------------------------------
## save
## -------------------------------------

write_csv(diag, file.path(est_dir, "stan_diagnostics.csv"))

################################################################################
## END SCRIPT
################################################################################
