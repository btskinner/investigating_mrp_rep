################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] poststratify.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 14 November 2019
##
################################################################################

## libraries
libs <- c("cmdstanr", "haven", "tidyverse", "rstan")
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

## -----------------------------------------------------------------------------
## FUNCTIONS
## -----------------------------------------------------------------------------

## convert parameters from MLM to predicted values
make_prediction <- function(school_num,
                            pars,
                            data,
                            design_matrix) {

    ## school_num := school numbers (group index)
    ## parameters := parameters from one model
    ## design_matrix := indicators for each group

    dmat <- design_matrix %>%
        filter(school_i == school_num) %>%
        arrange(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
        as.matrix

    ## second level (just need to do once)
    sl_lc <- c(t(data$z[school_num,]) %*% t(pars[["beta"]]))

    ## linear combination function, row by row
    make_lincom <- function(i) {
        pars[["beta_0"]] +
            pars[["sch_alpha"]][,dmat[[i,"school_i"]]] +
            pars[["gen_alpha"]][,dmat[[i,"gen_cat"]]] +
            pars[["rac_alpha"]][,dmat[[i,"rac_cat"]]] +
            pars[["age_alpha"]][,dmat[[i,"age_cat"]]] +
            pars[["gpa_alpha"]][,dmat[[i,"gpa_cat"]]] +
            pars[["hrs_alpha"]][,dmat[[i,"hrs_cat"]]] +
            sl_lc
    }

    ## prespecify output matrix (much faster)
    out_mat <- matrix(NA_real_, nrow(dmat), length(pars[["beta_0"]]))
    ## get linear combination for each design matrix row
    for (i in 1:nrow(out_mat)) {
        out_mat[i,] <- make_lincom(i)
    }
    ## inverse logit function to get predicted probabilities
    inv_logit(out_mat)
}

## wrapper function for make_prediction()
make_predictions <- function(question,
                             pars_list,
                             data_list,
                             design_matrix) {

    ## question := question name
    ## pars_list := parameters from Stan
    ## design_matrix := indicators for each group

    ## pull all model parameters for this question
    pars <- pars_list[[question]]

    ## pull all data for this question
    data <- data_list[[question]]

    ## pull vector of numbers == alphas (== # of schools)
    schools <- 1:ncol(pars[["sch_alpha"]])

    ## make predictions by school and then bind together
    message("    - ", question)
    out_list <- vector("list", length = length(schools))
    for (i in seq_along(schools)) {
        out_list[[i]] <- make_prediction(i, pars, data, design_matrix)
    }
    ## bind everything together
    reduce(out_list, rbind)
}

## -----------------------------------------------------------------------------
## INPUT
## -----------------------------------------------------------------------------

## -----------------------------------------------
## read in fit list
## -----------------------------------------------

full_fit_list <- readRDS(file.path(est_dir, "full_fit_list.rds"))

## -----------------------------------------------
## read in data list
## -----------------------------------------------

data_list <- get_rstan_data_list(path = dat_dir,
                                 pattern = "stan_dat_q*")

## -----------------------------------------------
## extract parameter draws
## -----------------------------------------------

## we "read" the output from each model output object and extract only
## those parameters that we want; should end up with a list of lists,
## with the parameters being in vectors or matrices
pars_list <- map(full_fit_list,
                 ~ extract(., pars = c("beta_0",
                                       "sch_alpha",
                                       "gen_alpha",
                                       "rac_alpha",
                                       "age_alpha",
                                       "gpa_alpha",
                                       "hrs_alpha",
                                       "beta")))

## -----------------------------------------------
## group counts and design matrix
## -----------------------------------------------

poststrat_mat <- read_dta(file.path(dat_dir, "poststrat_counts.dta"))
design_matrix <- poststrat_mat %>%
    arrange(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
    select(-count)
group_counts <- poststrat_mat %>%
    arrange(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
    pull(count)

## -----------------------------------------------------------------------------
## PREDICTIONS
## -----------------------------------------------------------------------------

## -----------------------------------------------
## get predictions for each question
## -----------------------------------------------

message("  Making predictions for question:")
preds_list <- map(names(pars_list),
                  ~ make_predictions(.x,
                                     pars_list,
                                     data_list,
                                     design_matrix)) %>%
    setNames(names(pars_list))

gc()
## save
saveRDS(preds_list, file.path(est_dir, "predictions.rds"))

## -----------------------------------------------------------------------------
## POSTRATIFY
## -----------------------------------------------------------------------------

## -----------------------------------------------
## OVERALL
## -----------------------------------------------

message("  Poststratifying: overall")

## reweighting at each level
ps_list <- map(preds_list,
               ~ ps_weight(.x, group_counts))

## ----------------------
## combined
## ----------------------

## q5_* | q4
ps_list_q5q4 <- map(1:5,
                    ~ ps_weight_2(preds_list[[paste0("q5_",.x)]],
                                  preds_list[["q4"]],
                                  group_counts)) %>%
    setNames(paste0("q5_", 1:5, "q4"))

## q6_5 | q4
ps_list_q6q4 <- map(5,
                    ~ ps_weight_2(preds_list[[paste0("q6_",.x)]],
                                  preds_list[["q4"]],
                                  group_counts)) %>%
    setNames("q6_5q4")

## q11_* | q10
ps_list_q11q10 <- map(1:5,
                      ~ ps_weight_2(preds_list[[paste0("q11_",.x)]],
                                    preds_list[["q10"]],
                                    group_counts)) %>%
     setNames(paste0("q11_", 1:5, "q10"))

## append
ps_list <- append(ps_list, ps_list_q5q4)
ps_list <- append(ps_list, ps_list_q6q4)
ps_list <- append(ps_list, ps_list_q11q10)

## ----------------------
## save
## ----------------------

saveRDS(ps_list, file.path(est_dir, "weighted_predictions.rds"))

## -----------------------------------------------
## BY SUBGROUP
## -----------------------------------------------

subgroups <- c("school_i", "gen_cat", "rac_cat", "age_cat", "gpa_cat", "hrs_cat")

for (sg in subgroups) {
    message(paste0("  Poststratifying: by ", sg))

    ## -----------------------------------------------
    ## overall within group
    ## -----------------------------------------------

    ## reweighting at each level
    ps_list <- map(preds_list,
                   ~ map(design_matrix %>% distinct(!!sym(sg)) %>% pull,
                         ~ ps_weight(.y[(design_matrix[[sg]] == .x),],
                                     group_counts[(design_matrix[[sg]] == .x)]),
                         .y = .x) %>%
                       do.call(rbind, .)) %>%
        setNames(names(preds_list))

    ## ----------------------
    ## combined
    ## ----------------------

    ## q5_* | q4
    ps_list_q5q4 <- map(1:5,
                        ~ map(design_matrix %>% distinct(!!sym(sg)) %>% pull,
                              ~ {
                                  ## get primary pred and subset to school
                                  x <- preds_list[[paste0("q5_", .y)]]
                                  x <- x[(design_matrix[[sg]] == .x),]
                                  ## get intermediate pred and subset to school
                                  y <- preds_list[["q4"]]
                                  y <- y[(design_matrix[[sg]] == .x),]
                                  ## subset group counts to schoool
                                  n <- group_counts[(design_matrix[[sg]] == .x)]
                                  ## poststratify
                                  ps_weight_2(x, y, n)
                              },
                              .y = .x) %>%
                            do.call(rbind, .)) %>%
        setNames(paste0("q5_", 1:5, "q4"))

    ## q6_* | q4
    ps_list_q6q4 <- map(5,
                        ~ map(design_matrix %>% distinct(!!sym(sg)) %>% pull,
                              ~ {
                                  ## get primary pred and subset to school
                                  x <- preds_list[[paste0("q6_", .y)]]
                                  x <- x[(design_matrix[[sg]] == .x),]
                                  ## get intermediate pred and subset to school
                                  y <- preds_list[["q4"]]
                                  y <- y[(design_matrix[[sg]] == .x),]
                                  ## subset group counts to schoool
                                  n <- group_counts[(design_matrix[[sg]] == .x)]
                                  ## poststratify
                                  ps_weight_2(x, y, n)
                              },
                              .y = .x) %>%
                            do.call(rbind, .))  %>%
        setNames("q6_5q4")

    ## q11_* | q10
    ps_list_q11q10 <- map(1:5,
                          ~ map(design_matrix %>% distinct(!!sym(sg)) %>% pull,
                                ~ {
                                    ## get primary pred and subset to school
                                    x <- preds_list[[paste0("q11_", .y)]]
                                    x <- x[(design_matrix[[sg]] == .x),]
                                    ## get intermediate pred and subset to school
                                    y <- preds_list[["q10"]]
                                    y <- y[(design_matrix[[sg]] == .x),]
                                    ## subset group counts to schoool
                                    n <- group_counts[(design_matrix[[sg]] == .x)]
                                    ## poststratify
                                    ps_weight_2(x, y, n)
                                },
                                .y = .x) %>%
                              do.call(rbind, .))  %>%
        setNames(paste0("q11_", 1:5, "q10"))

    ## append
    ps_list <- append(ps_list, ps_list_q5q4)
    ps_list <- append(ps_list, ps_list_q6q4)
    ps_list <- append(ps_list, ps_list_q11q10)

    ## ----------------------
    ## save
    ## ----------------------

    stub <- str_replace(sg, "(.+)_.+", "\\1")
    fn <- paste0("weighted_predictions_", stub, ".rds")
    saveRDS(ps_list, file.path(est_dir, fn))

}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
