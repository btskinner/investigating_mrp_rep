################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] make_tables.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 3 March 2020
##
################################################################################

## libraries
libs <- c("tidyverse", "haven", "xtable","Hmisc")
invisible(suppressMessages(sapply(libs, require, character.only = TRUE)))

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data", "clean")
est_dir <- file.path(root, "data", "estimates")
ext_dir <- file.path(root, "data", "external")
fig_dir <- file.path(root, "figures")
sta_dir <- file.path(root, "scripts", "stan")
scr_dir <- file.path(root, "scripts", "r")
tab_dir <- file.path(root, "tables")

## macros
source(file.path(scr_dir, "macros.R"))

## labels
source(file.path(scr_dir, "labels.R"))

## -----------------------------------------------------------------------------
## FUNCTIONS
## -----------------------------------------------------------------------------

## add parentheses
add_parens <- function(x) { gsub("(.+)", "(\\1)", x) }

## remove decimal
rm_dec <- function(x) { gsub("(.+)\\..+", "\\1", x) }

## add italics
add_italics <- function(x, em = 1) {
    gsub("(.+)", paste0("{\\\\itshape \\1}"), x)
}

## add bold
add_bold <- function(x) { gsub("(.*)", "{\\\\bfseries \\1}", x) }

## add hspace
add_hspace <- function(x) { gsub("(.*)", "\\\\hspace{1em}\\1", x) }

## ztest for difference in two proportions
tp_ztest <- function(p1, p2, y1, y2, n1, n2) {
    p <- (y1 + y2) / (n1 + n2)
    num <- p1 - p2
    den <- sqrt(p * (1 - p) * (1/n1 + 1/n2))
    z <- num / den
    2*pnorm(abs(z), lower.tail = FALSE)
}

## -----------------------------------------------------------------------------
## GET DATA
## -----------------------------------------------------------------------------

## first-level: cleaned Helios survey data
fl <- read_dta(file.path(dat_dir, "analysis_data.dta")) %>%
    ## using only those who at least completed q3_*
    filter(!is.na(q3_1)) %>%
    ## convert school_i to integer for better join later
    mutate(school_i = as.integer(school_i),
           age_cat = add_age_cats_short(age_cat),
           gen_cat = add_gen_cats(gen_cat),
           rac_cat = add_rac_cats_short(rac_cat),
           hrs_cat = add_hrs_cats_short(hrs_cat),
           gpa_cat = add_gpa_cats_short(gpa_cat))

## second-level: other sources
sl <- readRDS(file.path(dat_dir, "second_level_df.rds"))

## post-strat counts
pc <- read_dta(file.path(dat_dir, "poststrat_counts.dta")) %>%
    ## right_join(design_matrix) %>%
    ## mutate(count = ifelse(is.na(count), 0, count)) %>%
    mutate(age_cat = add_age_cats_short(age_cat),
           gen_cat = add_gen_cats(gen_cat),
           rac_cat = add_rac_cats_short(rac_cat),
           hrs_cat = add_hrs_cats_short(hrs_cat),
           gpa_cat = add_gpa_cats_short(gpa_cat))

## survey counts
ids <- read_dta(file.path(dat_dir, "main_crosswalk.dta")) %>%
    distinct(id) %>%
    pull

sc <- read_dta(file.path(dat_dir, "full_no_drop_demographics_data.dta")) %>%
    filter(id %in% ids) %>%
    distinct(id, .keep_all = TRUE) %>%
    ## convert school_i to integer for better join later
    mutate(school_i = as.integer(school_i),
           age_cat = add_age_cats_short(age_cat),
           gen_cat = add_gen_cats(gen_cat),
           rac_cat = add_rac_cats_short(rac_cat),
           hrs_cat = add_hrs_cats_short(hrs_cat),
           gpa_cat = add_gpa_cats_short(gpa_cat))

## figure data (poststratified posteriors)
pp <- readRDS(file.path(est_dir, "weighted_predictions.rds")) %>%
    bind_rows() %>%
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "prob") %>%
    group_by(question) %>%
    summarise(q50 = median(prob),
              q2_5 = quantile(prob, 0.025),
              q97_5 = quantile(prob, 0.975),
              .groups = "drop") %>%
    ungroup() %>%
    mutate(qlabs = add_qlabs(question),
           qcats = add_qcats(question),
           qlabs_plain = str_replace(qlabs, "(.*): (.*)", "\\2"),
           qlabs_plain = str_to_sentence(qlabs_plain)) %>%
    ## filter to relevant questions
    filter(grepl("q3|q5_.*q|q6|q7|q8|q9|q11_.*q|q12", question)) %>%
    ## drop unconditional question
    filter(!(question %in% c("q6_5"))) %>%
    left_join(fl %>%
              select(matches("q3|q5|q6|q7|q8|q9|q11|q12")) %>%
              mutate_all(~ as.integer(.)) %>%
              pivot_longer(cols = everything(),
                           names_to = "question",
                           values_to = "raw") %>%
              group_by(question) %>%
              summarise(raw_mean = mean(raw, na.rm = TRUE),
                        .groups = "drop") %>%
              mutate(question = str_replace(question, "(q5_.*)", "\\1q4"),
                     question = str_replace(question, "(q11_.*)", "\\1q10"),
                     question = str_replace(question, "(q6_5)", "\\1q4")),
              by = "question") %>%
    mutate(raw_mean_se = sqrt(raw_mean * (1 - raw_mean) / nrow(fl)),
           raw_mean_lo = raw_mean - qnorm(.975) * raw_mean_se,
           raw_mean_hi = raw_mean + qnorm(.975) * raw_mean_se) %>%
    mutate_if(is.numeric, function(x) round(x * 100, 1))


## -----------------------------------------------------------------------------
## DEMOGRAPHICS IN SURVEY VERSUS POSTSTRATIFIED POPULATION
## -----------------------------------------------------------------------------

demo_list <- c("age_cat", "gen_cat", "rac_cat", "gpa_cat", "hrs_cat")

out_mat <- map(demo_list,
               ~ {
                   ## survey
                   sur <- fl %>%
                       count(cat = !!sym(.x)) %>%
                       mutate(sprop = n / sum(n),
                              stot = sum(n),
                              var = .x) %>%
                       select(var, cat, sprop, sn = n, stot)

                   ## contacted
                   con <- sc %>%
                       count(cat = !!sym(.x)) %>%
                       filter(!is.na(cat)) %>%
                       mutate(cprop = n / sum(n),
                              ctot = sum(n),
                              var = .x) %>%
                       select(var, cat, cprop, cn = n, ctot)

                   ## population
                   pop <- pc %>%
                       group_by(cat = !!sym(.x)) %>%
                       summarise(n = sum(count),
                                 .groups = "drop") %>%
                       mutate(pprop = n / sum(n),
                              ptot = sum(n),
                              var = .x) %>%
                       select(var, cat, pprop, pn = n, ptot)

                   ## combine, compute z-score of difference, make sig stars, clean up
                   demo_1 <- left_join(sur, con, by = "cat") %>%
                       mutate(p_val = tp_ztest(sprop, cprop, sn, cn, stot, ctot),
                              stars = case_when(
                                  p_val <= 0.10 & p_val > 0.05 ~ "+",
                                  p_val <= 0.05 & p_val > 0.01 ~ "*",
                                  p_val <= 0.01 & p_val > 0.001 ~ "**",
                                  p_val <= 0.001 ~ "***",
                                  TRUE ~ ""),
                              spct = round(sprop * 100, 0),
                              ppct = round(cprop * 100, 0)) %>%
                       select(cat, spct1 = spct, ppct1 = ppct, stars1 = stars)

                   demo_2 <- left_join(sur, pop, by = "cat") %>%
                       mutate(p_val = tp_ztest(sprop, pprop, sn, pn, stot, ptot),
                              stars = case_when(
                                  p_val <= 0.10 & p_val > 0.05 ~ "+",
                                  p_val <= 0.05 & p_val > 0.01 ~ "*",
                                  p_val <= 0.01 & p_val > 0.001 ~ "**",
                                  p_val <= 0.001 ~ "***",
                                  TRUE ~ ""),
                              spct = round(sprop * 100, 0),
                              ppct = round(pprop * 100, 0)) %>%
                       select(cat, spct2 = spct, ppct2 = ppct, stars2 = stars)

                   left_join(demo_1, demo_2, by = "cat")
               }
               ) %>%
    setNames(demo_list) %>%
    bind_rows %>%
    as.matrix

## add horizontal space to all left column names (prepare for headers)
out_mat[,1] <- add_hspace(out_mat[,1])

## add headers
out_mat <- rbind(cbind("Age group", rbind(rep("", 6))),
                 out_mat[1:4,],
                 cbind("Gender", rbind(rep("", 6))),
                 out_mat[5:7,],
                 cbind("Race/ethnicity", rbind(rep("", 6))),
                 out_mat[8:13,],
                 cbind("GPA", rbind(rep("", 6))),
                 out_mat[14:19,],
                 cbind("Earned credit hours", rbind(rep("", 6))),
                 out_mat[20:25,])

## add bold to headers
out_mat[c(1,6,10,17,24),1] <- add_bold(out_mat[c(1,6,10,17,24),1])

## add Ns row at bottom of matrix
out_mat <- rbind(out_mat,
                 cbind("$N$",
                       nrow(fl),
                       nrow(sc),
                       "",
                       nrow(fl),
                       pc %>% pull(count) %>% sum,
                       ""))

## table notes
notes <- c(paste0("$+ p < 0.1$, $^{*} p < 0.05$, $^{**} p < 0.01$, ",
                  "$^{***} p < 0.001$. All numbers are percentages. ",
                  "The contacted (2) sample represents the subset of the ",
                  "population (5) sample with an active cell phone number on file. ",
                  "Columns (3) and (6) indicate the level of statistical ",
                  "significance of differences between the contacted and respondent ",
                  "samples and the population and respondent samples, respectively. ",
                  "Square brackets and parentheses ",
                  "around GPA intervals are inclusive and exclusive, ",
                  "respectively."))

## header
head <- c("\\begin{table}[!ht]",
          "\\footnotesize",
          "\\centering",
          "\\caption{Comparison of respondents to those who were contacted ",
          "and the population of interest across characteristics}",
          "\\label{tab:demo_tab}",
          "\\begin{tabularx}{\\linewidth}{Xcccccc}",
          "\\toprule",
          "&Respondent&Contacted& Sig. &Respondent&Population& Sig. \\\\",
          ## paste(paste0("\\cmidrule(lr){", 2:7, "-", 2:7 ,"}"), collapse = ""),
          "\\cmidrule(lr){2-7}",
          paste(c(paste0("&(",1:6,")"), "\\\\"), collapse = ""))

## primary contents
contents <- print(xtable(out_mat),
                  booktabs = TRUE,
                  sanitize.text.function = function(x){x},
                  include.colnames = FALSE,
                  include.rownames = FALSE,
                  only.contents = TRUE,
                  print.results = FALSE,
                  hline.after = c(-1, 0, nrow(out_mat) - 1),
                  comment = FALSE)

## footer
foot <- c("\\bottomrule",
          "\\multicolumn{7}{p{.98\\linewidth}}{\\footnotesize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{tabularx}",
          "\\end{table}")

## write out table in TeX
writeLines(c(head, contents, foot), con = file.path(tab_dir, "demo_tab.tex"))

## -----------------------------------------------------------------------------
## EXAMPLE TABLE OF POSTSTRAT CATEGORIES AND WEIGHTS
## -----------------------------------------------------------------------------

out_mat <- rbind(pc %>%
                 filter(count > 10) %>%
                 arrange(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
                 head(n = 5) %>%
                 as.matrix,
                 rbind(rep("$\\ldots$", ncol(pc))),
                 pc %>%
                 filter(count > 10) %>%
                 arrange(school_i, gen_cat, rac_cat, age_cat, gpa_cat, hrs_cat) %>%
                 tail(n = 5) %>%
                 as.matrix)

## bold last column of numbers
out_mat[,7] <- add_bold(out_mat[,7])

## table notes
notes <- c(paste0("The full poststratification table contains counts for 5 ",
                  "schools; 3 genders: men, women, and missing; ",
                  "6 race/ethnicities: Black, Hispanic, more than one ",
                  "race/ethnicity, white, other racial/ethnic groups, and ",
                  "missing; ",
                  "4 age groups: 18-25, 26-35, 36-49, and 50+; 6 GPA bins: ",
                  "[2.0-2.3), [2.3-2.7), [2.7-3.0), [3.0-3.3), [3.3-3.7), and ",
                  "[3.7-4.0); and 6 earned credit hour bins: 30-35, 36-41, 42-47, ",
                  "48-53, 54-59, and 60+ hours."))


## header
head <- c("\\begin{table}[!ht]",
          "\\centering",
          "\\caption{Example poststratification cell counts}",
          "\\label{tab:count_tab}",
          "\\begin{tabularx}{\\linewidth}{cXXXXXc}",
          "\\toprule",
          "School&Gender&Race/ethnicity&Age&GPA&Credit hours&{\\bfseries Count} \\\\")

## primary contents
contents <- print(xtable(out_mat),
                  booktabs = TRUE,
                  sanitize.text.function = function(x){x},
                  include.colnames = FALSE,
                  include.rownames = FALSE,
                  only.contents = TRUE,
                  print.results = FALSE,
                  comment = FALSE)

## footer
foot <- c("\\multicolumn{7}{p{.98\\linewidth}}{\\footnotesize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{tabularx}",
          "\\end{table}")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "count_tab.tex"))

## -----------------------------------------------------------------------------
## POSTSTRATIFIED POSTERIOR TABLE
## -----------------------------------------------------------------------------

## get rough arrangement of table
pp_tab <- pp %>%
    arrange(qcats, -q50) %>%
    mutate(ppci95 = paste0("[", q2_5, ", ", q97_5, "]"),
           rmci95 = paste0("(", raw_mean_lo, ", ", raw_mean_hi, ")")) %>%
    select(qcats, qlabs_plain, q50, ppci95, raw_mean, rmci95) %>%
    mutate_at(vars(q50, raw_mean), as.character) %>%
    pivot_longer(q50:ppci95,
                 names_to = "post",
                 values_to = "post_val") %>%
    pivot_longer(raw_mean:rmci95,
                 names_to = "rm",
                 values_to = "rm_val") %>%
    filter((post == "q50" & rm == "raw_mean") |
           (post == "ppci95" & rm == "rmci95")) %>%
    mutate(qlabs_plain = ifelse(row_number() %% 2 == 0, "", qlabs_plain)) %>%
    select(qcats, qlabs_plain, rm_val, post_val)

## put into matrix
out_mat <- rbind(cbind("Cost", rbind(rep("", ncol(pp_tab) - 2))),
                 pp_tab %>%
                 filter(qcats == "Cost") %>%
                 select(-qcats) %>%
                 as.matrix,
                 ## Employment
                 cbind("Employment", rbind(rep("", ncol(pp_tab) - 2))),
                 pp_tab %>%
                 filter(qcats == "Employment") %>%
                 select(-qcats) %>%
                 as.matrix,
                 ## Instructional
                 cbind("Instructional", rbind(rep("", ncol(pp_tab) - 2))),
                 pp_tab %>%
                 filter(qcats == "Instructional") %>%
                 select(-qcats) %>%
                 as.matrix,
                 ## Other
                 cbind("Other", rbind(rep("", ncol(pp_tab) - 2))),
                 pp_tab %>%
                 filter(qcats == "Other") %>%
                 select(-qcats) %>%
                 as.matrix)

## add bold
index <- c(1,24,35,66)
out_mat[index,1] <- add_bold(out_mat[index,1])

## add hspace
index <- setdiff(1:nrow(out_mat), index)
out_mat[index,1] <- add_hspace(out_mat[index,1])

## table notes
notes <- c(paste0("Survey mean estimates are the number of clickes (positive ",
                  "selections) divided by total views. The 95\\% confidence ",
                  "intervals for the means are reported in parentheses. ",
                  "Bayesian point estimates are poststratified medians, with ",
                  "95\\% credible intervals reported in square brackets."))

## header
head <- c("\\begin{longtable}{lcc}",
          "\\caption{Percentage of students indicating reason for early exit} \\\\",
          "\\label{tab:ps_tab} \\\\",
          "\\toprule",
          "&Survey mean&Poststratified median \\\\",
          "\\endfirsthead",
          "\\multicolumn{3}{l}{\\emph{...table \\thetable{} continued}} \\\\",
          "\\toprule",
          "&Survey mean&Poststratified median \\\\",
          "\\midrule",
          "\\endhead",
          "\\bottomrule",
          "\\multicolumn{3}{r}{\\emph{Continued on next page...}} \\\\",
          "\\endfoot",
          "\\endlastfoot")

## primary contents
contents <- print(xtable(out_mat),
                  booktabs = TRUE,
                  sanitize.text.function = function(x){x},
                  include.colnames = FALSE,
                  include.rownames = FALSE,
                  only.contents = TRUE,
                  floating = FALSE,
                  tabular.environment = "longtable",
                  print.results = FALSE,
                  comment = FALSE)

## footer
foot <- c("\\multicolumn{3}{p{.98\\linewidth}}{\\footnotesize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{longtable}")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "ps_tab.tex"))

## -----------------------------------------------------------------------------
## IPEDS COMPARISON
## -----------------------------------------------------------------------------

## add horizontal space
add_hspace <- function(x, em = 1) {
    gsub("(.+)", paste0("\\\\hspace{", em, "em}\\1"), x)
}

## read in unitid list if it exisits
if (file.exists(file.path(ext_dir, "unitid_list.RDS"))) {
    unitid_list <- readRDS(file.path(ext_dir, "unitid_list.RDS"))
} else {
    ## unitid for FCS
    fcs_unitid <- c("132709", "133021", "132851", "133386", "132693",
                    "135160", "133508", "133702", "134343", "134495",
                    "134608", "135188", "135717", "136145", "136233",
                    "136358", "136400", "136473", "136516", "137281",
                    "137096", "137209", "137315", "137078", "135391",
                    "137759", "133960", "138187")
    ## for the RCT
    rct_unitid <- c("132709", "134495", "135717", "136358", "138187")
    ## HD file
  hd <- read_csv(file.path(ext_dir,
                           unzip(file.path(ext_dir, "HD2019.zip"), "hd2019.csv",
                                 exdir = ext_dir)),
                 show_col_types = FALSE) |>
        rename_all(tolower) |>
        filter(carnegie == 40, control == 1, pseflag == 1)
    ## unitid for all associate's universities
    all_unitid <- hd |> pull(unitid)
    ## store in list
    unitid_list <- list("rct" = rct_unitid,
                        "fcs" = fcs_unitid,
                        "all" = all_unitid)
    ## save for the future
    saveRDS(unitid_list, file.path(ext_dir, "unitid_list.RDS"))
}

## EF
if (file.exists(file.path(ext_dir, "ef.RDS"))) {
    ef <- readRDS(file.path(ext_dir, "ef.RDS"))
} else {
  ef <- read_csv(file.path(ext_dir,
                           unzip(file.path(ext_dir, "EFFY2019.zip"), "effy2019_rv.csv",
                                 exdir = ext_dir)),
                 show_col_types = FALSE) |>
    rename_all(tolower) |>
    filter(unitid %in% unitid_list[["all"]], effylev == 2) |>
    select(-starts_with("x")) |>
        mutate(men_pct = efytotlm / efytotlt * 100,
               aian_pct = efyaiant / efytotlt * 100,
               asia_pct = efyasiat / efytotlt * 100,
               bkaa_pct = efybkaat / efytotlt * 100,
               hisp_pct = efyhispt / efytotlt * 100,
               nhpi_pct = efynhpit / efytotlt * 100,
               whit_pct = efywhitt / efytotlt * 100,
               tmor_pct = efy2mort / efytotlt * 100) |>
        select(unitid, efytotlt, ends_with("pct"))
    ## save for future
    saveRDS(ef, file.path(ext_dir, "ef.RDS"))
}

## SFA file
if (file.exists(file.path(ext_dir, "sf.RDS"))) {
    sf <- readRDS(file.path(ext_dir, "sf.RDS"))
} else {
  sf <- read_csv(file.path(ext_dir,
                           unzip(file.path(ext_dir, "SFA1819.zip"), "sfa1819.csv",
                                 exdir = ext_dir)),
                 show_col_types = FALSE) |>
    rename_all(tolower) |>
        filter(unitid %in% unitid_list[["all"]]) |>
        select(unitid, npist2, upgrnta, upgrntp, anyaidp)
    ## save for future
    saveRDS(sf, file.path(ext_dir, "sf.RDS"))
}

## GR file
if (file.exists(file.path(ext_dir, "gr.RDS"))) {
    gr <- readRDS(file.path(ext_dir, "gr.RDS"))
} else {
  gr <- read_csv(file.path(ext_dir,
                           unzip(file.path(ext_dir, "GR2019.zip"), "gr2019.csv",
                                 exdir = ext_dir)),
                 show_col_types = FALSE) |>
        rename_all(tolower) |>
        filter(unitid %in% unitid_list[["all"]],
               line == "29A" | line == "50") |>
        select(unitid, line, grtotlt, grtotlm, graiant, grasiat,
               grbkaat, grhispt, grnhpit, grwhitt, gr2mort) |>
        group_by(unitid, line) |>
        summarise(across(grtotlt:gr2mort, mean), .groups = "drop") |>
        pivot_wider(id_cols = "unitid",
                    names_from = "line",
                    values_from = c(grtotlt, grtotlm, graiant, grasiat,
                                    grbkaat, grhispt, grnhpit, grwhitt,
                                    gr2mort)) |>
        mutate(tot_gr_pct = grtotlt_29A / grtotlt_10 * 100,
               men_gr_pct = grtotlm_29A / grtotlm_10 * 100,
               aian_gr_pct = graiant_29A / graiant_10 * 100,
               asia_gr_pct = grasiat_29A / grasiat_10 * 100,
               bkaa_gr_pct = grbkaat_29A / grbkaat_10 * 100,
               hisp_gr_pct = grhispt_29A / grhispt_10 * 100,
               nhpi_gr_pct = grnhpit_29A / grnhpit_10 * 100,
               whit_gr_pct = grwhitt_29A / grwhitt_10 * 100,
               tmor_gr_pct = gr2mort_29A / gr2mort_10 * 100) |>
        select(unitid, ends_with("pct")) |>
        mutate(across(ends_with("pct"), ~ replace(.x, is.nan(.x), 0)))
    ## save for future
    saveRDS(gr, file.path(ext_dir, "gr.RDS"))
}


## Completions file
if (file.exists(file.path(ext_dir, "cp.RDS"))) {
    cp <- readRDS(file.path(ext_dir, "cp.RDS"))
} else {
  cp <- read_csv(file.path(ext_dir,
                           unzip(file.path(ext_dir, "C2019_C.zip"), "c2019_c_rv.csv",
                                 exdir = ext_dir)),
                 show_col_types = FALSE) |>
        rename_all(tolower) |>
        filter(unitid %in% unitid_list[["all"]], awlevelc == "03") |>
        select(-starts_with("x")) |>
        mutate(men_cs_pct = cstotlm / cstotlt * 100,
               aian_cs_pct = csaiant / cstotlt * 100,
               asia_cs_pct = csasiat / cstotlt * 100,
               bkaa_cs_pct = csbkaat / cstotlt * 100,
               hisp_cs_pct = cshispt / cstotlt * 100,
               nhpi_cs_pct = csnhpit / cstotlt * 100,
               whit_cs_pct = cswhitt / cstotlt * 100,
               tmor_cs_pct = cs2mort / cstotlt * 100,
               cs18_24_pct = cs18_24 / cstotlt * 100,
               cs25_39_pct = cs25_39 / cstotlt * 100,
               csabv40_pct = csabv40 / cstotlt * 100) |>
        select(unitid, cstotlt, ends_with("pct"))
    ## save for future
    saveRDS(cp, file.path(ext_dir, "cp.RDS"))
}

## get values: sample, FCS, nation
out <- map(unitid_list,
           ~ {
               ## unitids to subset
               ids <- unlist(.x)

               ## enrollment
               enroll <- ef |>
                   filter(unitid %in% ids) |>
                   summarise(across(c(efytotlt,
                                      ends_with("pct")),
                                    list(mean = ~ mean(.x, na.rm = TRUE),
                                         sd = ~ sd(.x, na.rm = TRUE)))) |>
                   pivot_longer(cols = everything(),
                                names_to = "stat",
                                values_to = "val") |>
                   mutate(var = "enrol",
                          sam = names(.x))

               ## costs
               cost <- sf |>
                   filter(unitid %in% ids) |>
                   summarise(across(c(npist2:anyaidp),
                                    list(mean = ~ mean(.x, na.rm = TRUE),
                                         sd = ~ sd(.x, na.rm = TRUE)))) |>
                   pivot_longer(cols = everything(),
                                names_to = "stat",
                                values_to = "val") |>
                   mutate(var = "cost",
                          sam = names(.x))

               ## completions
               comp <- cp |>
                   filter(unitid %in% ids) |>
                   summarise(across(c(cstotlt,
                                      ends_with("pct")),
                                    list(mean = ~ mean(.x, na.rm = TRUE),
                                         sd = ~ sd(.x, na.rm = TRUE)))) |>
                   pivot_longer(cols = everything(),
                                names_to = "stat",
                                values_to = "val") |>
                   mutate(var = "comp",
                          sam = names(.x))

               ## graduation rates
               grad <- gr |>
                   filter(unitid %in% ids) |>
                   summarise(across(c(ends_with("pct")),
                                    list(mean = ~ mean(.x, na.rm = TRUE),
                                         sd = ~ sd(.x, na.rm = TRUE)))) |>
                   pivot_longer(cols = everything(),
                                names_to = "stat",
                                values_to = "val") |>
                   mutate(var = "grad",
                          sam = names(.x))

               ## bind together
               bind_rows(enroll, cost, comp, grad)

           }
           )

## number of schools in sample
n <- sapply(unitid_list, length)

## make full output matrix (contents of both tables)
mat <- cbind(out$rct$val,
             out$fcs$val,
             out$all$val)

odd <- seq(1,nrow(mat),2)
eve <- seq(2,nrow(mat),2)
wnm <- c(1,2,19,20,21,22,27,28)

## round
mat[odd,] <- round(mat[odd,],2)
mat[eve,] <- round(mat[eve,],2)
mat[wnm,] <- rm_dec(mat[wnm,])
mat[eve,] <- add_parens(mat[eve,])

## convert IPEDS names to nicer table names
fix_names <- function(x) {
    case_when(
        grepl("efytotlt", x) ~ "Total",
        grepl("men", x) ~ "Men (\\%)",
        grepl("aian", x) ~ "AI/AN (\\%)",
        grepl("asia", x) ~ "Asian (\\%)",
        grepl("bkaa", x) ~ "Black (\\%)",
        grepl("hisp", x) ~ "Hispanic (\\%)",
        grepl("nhpi", x) ~ "NH/PI (\\%)",
        grepl("whit", x) ~ "White (\\%)",
        grepl("tmor", x) ~ "More than one race (\\%)",
        grepl("cs18_24", x) ~ "18-24 (\\%)",
        grepl("cs25_39", x) ~ "25-39 (\\%)",
        grepl("csabv40", x) ~ "40+ (\\%)",
        grepl("npist2", x) ~ "Net price (\\$)",
        grepl("upgrntp", x) ~ "Pell grant recipients (\\%)",
        grepl("upgrnta", x) ~ "Average grant aid (\\$)",
        grepl("anyaidp", x) ~ "Any aid recipients (\\%)",
        grepl("cstotlt", x) ~ "Total",
        grepl("tot_gr", x) ~ "Overall (\\%)",
        grepl(".+_sd", x) ~ ""
    )
}

## core table
mat <- cbind(fix_names(out$rct$stat), mat)

## add horizontal space
mat[,1] <- add_hspace(mat[,1])

## remove even rows b/c those are sd rows
mat[eve,1] <- ""

## ---------------------------
## admissions table
## ---------------------------

## subset table
mat_1 <- rbind(c("{\\itshape Enrollments}", rep("",3)),
               mat[1:18,],
               c("{\\itshape Costs}", rep("",3)),
               mat[19:26,])

## core table
content_1 <- print(xtable(mat_1),
                   booktabs = TRUE,
                   sanitize.text.function = function(x){x},
                   include.colnames = FALSE,
                   include.rownames = FALSE,
                   only.contents = TRUE,
                   print.results = FALSE,
                   comment = FALSE)

header_1 <- c("\\begin{table}[!ht]",
              "\\caption{Comparison of enrollments and costs between our sample ",
              "institutions, all Florida College System institutions, and all ",
              "Associate's level institutions in the United States}",
              "\\label{tab:adcost}",
              "\\footnotesize",
              "\\begin{tabularx}{\\linewidth}{Xccc}",
              "\\toprule",
              "& Sites & Florida College System & National \\\\",
              "\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}",
              "& (1) & (2) & (3) \\\\")

footer_1 <- c(paste0("$N$ &", n[1], "&", n[2], "&", n[3], "\\\\"),
              "\\bottomrule",
              "\\multicolumn{4}{p{.98\\linewidth}}",
              "{\\footnotesize",
              "{\\itshape Note.} Estimates are averages within each sample, with",
              "standard deviations in parentheses. All data come from the Integrated",
              "Postsecondary Education Data System in the 2019 calendar year or",
              "2018/2019 academic year. Values in column are (1) are limited to the",
              "RCT sample (5 FCS colleges); values in column (2) represent all",
              "institutions in the Florida College System; values in column (3)",
              "represent all public Associate's level colleges in the United States.",
              "}",
              "\\end{tabularx}",
              "\\end{table}")

writeLines(c(header_1, content_1, footer_1), con = file.path(tab_dir, "ipeds1.tex"))

## ---------------------------
## completions table
## ---------------------------

## subset table
mat_2 <- rbind(c("{\\itshape Completions}", rep("",3)),
               mat[27:50,],
               c("{\\itshape Graduation rates (150\\%)}", rep("",3)),
               mat[51:68,])

## core table
content_2 <- print(xtable(mat_2),
                   booktabs = TRUE,
                   sanitize.text.function = function(x){x},
                   include.colnames = FALSE,
                   include.rownames = FALSE,
                   only.contents = TRUE,
                   print.results = FALSE,
                   comment = FALSE)

header_2 <- c("\\begin{table}[!ht]",
              "\\caption{Comparison of completions and graduation rates ",
              "between our sample ",
              "institutions, all Florida College System institutions, and all ",
              "Associate's level institutions in the United States}",
              "\\label{tab:gradrate}",
              "\\footnotesize",
              "\\begin{tabularx}{\\linewidth}{Xccc}",
              "\\toprule",
              "& Sites & Florida College System & National \\\\",
              "\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}",
              "& (1) & (2) & (3) \\\\")

footer_2 <- c(paste0("$N$ &", n[1], "&", n[2], "&", n[3], "\\\\"),
              "\\bottomrule",
              "\\multicolumn{4}{p{.98\\linewidth}}",
              "{\\footnotesize",
              "{\\itshape Note.} Estimates are averages within each sample, with",
              "standard deviations in parentheses. All data come from the Integrated",
              "Postsecondary Education Data System in the 2019 calendar year or",
              "2018/2019 academic year. Values in column are (1) are limited to the",
              "RCT sample (5 FCS colleges); values in column (2) represent all",
              "institutions in the Florida College System; values in column (3)",
              "represent all public Associate's level colleges in the United States.",
              "}",
              "\\end{tabularx}",
              "\\end{table}")

writeLines(c(header_2, content_2, footer_2), con = file.path(tab_dir, "ipeds2.tex"))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
