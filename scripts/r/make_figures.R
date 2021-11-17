################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] make_figures.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 15 November 2019
##
################################################################################

## libraries
libs <- c("haven", "tidyverse", "rstan", "scales", "ggridges", "patchwork")
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

## macros
source(file.path(scr_dir, "macros.R"))

## labels
source(file.path(scr_dir, "labels.R"))

## data
helios_clean <- "analysis_data.dta"

## colors
blue_hi <- "#6C9AC3FF"
blue_mi <- "#6C9AC3CC"
blue_lo <- "#6C9AC34D"
orange_hi <- "#E28F41FF"
orange_mi <- "#E28F41CC"
orange_lo <- "#E28F414D"

## plot settings
pl_device <- "pdf"
pl_path <- fig_dir
pl_width <- 9
pl_height <- 5.5
pl_scale <- 1.5
pl_units <- "in"
pl_dpi <- "retina"

## -----------------------------------------------------------------------------
## get weighted posteriors
## -----------------------------------------------------------------------------

## ---------------------------
## overall
## ---------------------------

df <- readRDS(file.path(est_dir, "weighted_predictions.rds")) %>%
    bind_rows() %>%
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "prob") %>%
    group_by(question) %>%
    mutate(q50 = median(prob)) %>%
    ungroup() %>%
    arrange(q50) %>%
    mutate(qlabs = add_qlabs(question),
           qcats = add_qcats(question),
           qlabs_plain = str_replace(qlabs, "(.*): (.*)", "\\2"),
           qlabs_plain = str_to_sentence(qlabs_plain)) %>%
    ## filter to relevant questions
    filter(grepl("q3|q5_.*q|q6|q7|q8|q9|q11_.*q|q12", question)) %>%
    ## drop unconditional question
    filter(!(question %in% c("q6_5")))

## ---------------------------
## split within groups
## ---------------------------

## grouping variables
subgroups <- c("gen_cat", "rac_cat", "age_cat", "gpa_cat", "hrs_cat")

## munge one grouping at a time and put into named list
df_group_list <- map(subgroups,
                 ~ {
                     ## assign variable name to sg
                     sg <- .x
                     ## get stub (everything in varname before _)
                     stub <- str_replace(sg, "(.+)_.+", "\\1")
                     ## make file name for read in
                     fn <- paste0("weighted_predictions_", stub, ".rds")
                     ## read in grouped weight posterior
                     readRDS(file.path(est_dir, fn)) %>%
                         ## index map to keep track of question names ("q10")
                         imap(.,
                              ## start with matrix (rows: cats; cols: draw)
                              ~ {
                                  ## set column names
                                  colnames(.x) <- paste0("d", 1:ncol(.x))
                                  ## convert to tibble
                                  as_tibble(.x) %>%
                                  ## add column that is row number, which == cat val
                                  mutate(!!sym(sg) := row_number()) %>%
                                  ## make long
                                  pivot_longer(cols = -!!sym(sg),
                                               names_to = "question",
                                               values_to = "prob") %>%
                                  ## convert V# to just the question name
                                      mutate(question = .y)
                              }) %>%
                         ## after all questions, bind together
                         bind_rows() %>%
                         ## group by category and question
                         group_by(!!sym(sg), question) %>%
                         ## get median value for factor ranking when plotting
                         mutate(q50 = median(prob)) %>%
                         ## ungroup to make faster!
                         ungroup() %>%
                         ## no real need, but easier to look at later
                         arrange(!!sym(.x), q50) %>%
                         ## add question and category labels for plotting
                         mutate(qlabs = add_qlabs(question),
                                qcats = add_qcats(question),
                                qlabs_plain = str_replace(qlabs, "(.*): (.*)", "\\2"),
                                qlabs_plain = str_to_sentence(qlabs_plain)) %>%
                         ## filter to relevant questions
                         filter(grepl("q3|q5_.*q|q6|q7|q8|q9|q11_.*q|q12", question)) %>%
                         ## drop unconditional question
                         filter(!(question %in% c("q6_5")))
                 }) %>%
    ## make sure each group has correct name
    setNames(subgroups)

## -----------------------------------------------------------------------------
## plots
## -----------------------------------------------------------------------------

## -------------------------------------
## overall
## -------------------------------------

## labels/captions
ptitle <- "Factors contributing to early exit from college"

## -----------------
## lines
## -----------------

plot_df <- df %>%
    ## group by question
    group_by(question) %>%
    ## get other quantiles
    mutate(q025 = quantile(prob, 0.025),
           q25 = quantile(prob, 0.25),
           q75 = quantile(prob, 0.75),
           q975 = quantile(prob, 0.975)) %>%
    ## drop probs
    select(-prob) %>%
    ## filter to distinct values
    distinct() %>%
    ## ungroup
    ungroup() %>%
    ## get highest poststratified median
    arrange(q50) %>%
    ## labels are "(original rank #) <label>"
    mutate(q = row_number(),
           q = factor(q,
                      levels = q,
                      labels = qlabs_plain))

## color by question category
g <- ggplot(plot_df) +
    geom_segment(aes(y = q, yend = q, x = q025, xend = q975, colour = qcats),
                 size = .5, show.legend = FALSE) +
    geom_segment(aes(y = q, yend = q, x = q25, xend = q75, colour = qcats),
                 size = 1.5, show.legend = FALSE) +
    geom_point(aes(y = q, x = q50, shape = qcats),
               size = 2.8, fill = "white", colour = "white") +
    geom_point(aes(y = q, x = q50, shape = qcats, colour = qcats),
                   fill = "white", size = 2.8) +
    scale_y_discrete(position = "right") +
    scale_x_continuous(breaks = seq(0, 1, 0.05),
                       labels = percent_format(accuracy = 1L)) +
    scale_shape_manual(name = "Question category",
                       values = 21:24) +
    scale_colour_discrete(name = "Question category") +
    coord_cartesian(xlim = c(0, 0.600001),
                    ylim = c(0, 42),
                    clip = "off",
                    expand = FALSE) +
    labs(x = "Poststratified percentage of students selecting factor",
         y = NULL,
         title = ptitle,
         subtitle = NULL) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(hjust = 0),
          text = element_text(size = 14),
          legend.justification = c(0,1),
          legend.position = c(0.01,0.99))

## save plot
ggsave(filename = paste0("ranked_posterior_overall_lines.", pl_device),
       plot = g,
       device = pl_device,
       path = pl_path,
       width = pl_width,
       height = pl_height,
       scale = pl_scale,
       units = pl_units,
       dpi = pl_dpi)

## -------------------------------------
## by subgroups
## -------------------------------------

## labels/captions
ptitle <- "Factors contributing to early exit from college"

walk(names(df_group_list),
     ~ {
         ## which category group are we focusing on
         cat <- .x
         walk(distinct(df_group_list[[cat]][cat]) %>% pull,
              ~ {
                  ## which specific subgroup within the cateogory
                  val <- .x
                  ## function label
                  lab_func <- get(add_cats_list[str_detect(add_cats_list, cat)][[1]])

                  ## -----------------
                  ## lines
                  ## -----------------

                  plot_df <- df_group_list[[cat]] %>%
                      filter(!!sym(cat) == val) %>%
                      ## group by question
                      group_by(question) %>%
                      ## get other quantiles
                      mutate(q025 = quantile(prob, 0.025),
                             q25 = quantile(prob, 0.25),
                             q75 = quantile(prob, 0.75),
                             q975 = quantile(prob, 0.975)) %>%
                      ## drop probs
                      select(-prob) %>%
                      ## filter to distinct values
                      distinct() %>%
                      ## ungroup
                      ungroup() %>%
                      ## get highest poststratified median
                      arrange(q50) %>%
                      ## labels are "(original rank #) <label>"
                      mutate(q = row_number(),
                             q = factor(q,
                                        levels = q,
                                        labels = qlabs_plain),
                             !!sym(cat) := lab_func(!!sym(cat)))

                  ## color by question category
                  g <- ggplot(plot_df) +
                      geom_segment(aes(y = q, yend = q, x = q025,
                                       xend = q975, colour = qcats),
                                   size = .5, show.legend = FALSE) +
                      geom_segment(aes(y = q, yend = q, x = q25,
                                       xend = q75, colour = qcats),
                                   size = 1.5, show.legend = FALSE) +
                      geom_point(aes(y = q, x = q50, shape = qcats),
                                 size = 2.8, fill = "white", colour = "white") +
                      geom_point(aes(y = q, x = q50, shape = qcats,
                                     colour = qcats),
                                 fill = "white", size = 2.8) +
                      scale_y_discrete(position = "right") +
                      scale_x_continuous(breaks = seq(0, 1, 0.05),
                                         labels = percent_format(accuracy = 1L)) +
                      scale_shape_manual(name = "Question category",
                                         values = 21:24) +
                      scale_colour_discrete(name = "Question category") +
                      coord_cartesian(xlim = c(0, max(plot_df$q975) + 0.01),
                                      ylim = c(0, 42),
                                      clip = "off",
                                      expand = FALSE) +
                      labs(x = "Poststratified percentage of students selecting factor",
                           y = NULL,
                           title = ptitle,
                           subtitle = NULL) +
                      theme_bw() +
                      theme(panel.grid.minor.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.x = element_text(hjust = 0),
                            text = element_text(size = 14),
                            legend.justification = c(0,1),
                            legend.position = c(0.01,0.99))

                  ## save plot
                  ggsave(filename = paste0("ranked_posterior_",
                                           cat,
                                           "_",
                                           val,
                                           "_lines.",
                                           pl_device),
                         plot = g,
                         device = pl_device,
                         path = pl_path,
                         width = pl_width,
                         height = pl_height,
                         scale = pl_scale,
                         units = pl_units,
                         dpi = pl_dpi)

              }
              )
     }
     )

## -------------------------------------
## by question
## -------------------------------------

## get outcomes in a vector
outcomes <- df %>%
    ## get distinct question names
    distinct(question) %>%
    ## pull to vector
    pull(question)

## make individual plots
plots <- map(outcomes,
             ~ {
                 plot_df <- bind_rows(df_group_list[["gen_cat"]] %>%
                                      filter(question == .x) %>%
                                      mutate(cat = gen_cat,
                                             var = "gen_cat") %>%
                                      select(-gen_cat) %>%
                                      mutate(cat_name = add_gen_cats(cat)) %>%
                                      filter(str_detect(cat_name,
                                                        "Missing",
                                                negate = TRUE)),

                                      df_group_list[["rac_cat"]] %>%
                                      filter(question == .x) %>%
                                      mutate(cat = rac_cat,
                                             var = "rac_cat") %>%
                                      select(-rac_cat) %>%
                                      mutate(cat_name = add_rac_cats_short(cat)) %>%
                                      filter(str_detect(cat_name,
                                                        "Missing",
                                                        negate = TRUE)),

                                      df_group_list[["age_cat"]] %>%
                                      filter(question == .x) %>%
                                      mutate(cat = age_cat,
                                             var = "age_cat") %>%
                                      select(-age_cat) %>%
                                      mutate(cat_name = add_age_cats_short(cat)),

                                      df_group_list[["gpa_cat"]] %>%
                                      filter(question == .x) %>%
                                      mutate(cat = gpa_cat,
                                             var = "gpa_cat") %>%
                                      select(-gpa_cat) %>%
                                      mutate(cat_name = add_gpa_cats_short(cat)),

                                      df_group_list[["hrs_cat"]] %>%
                                      filter(question == .x) %>%
                                      mutate(cat = hrs_cat,
                                             var = "hrs_cat") %>%
                                      select(-hrs_cat) %>%
                                      mutate(cat_name = add_hrs_cats_short(cat))

                                      ) %>%
                     mutate(cat_lev = factor(cat_name))

                 ## labels/captions
                 ptitle <- paste0(plot_df %>% distinct(qlabs_plain) %>% pull)

                 ## facet labels
                 facet_labels <- c("gen_cat" = "Gender",
                                   "rac_cat" = "Race/ethnicity",
                                   "age_cat" = "Age",
                                   "gpa_cat" = "GPA",
                                   "hrs_cat" = "Earned credit hours")

                 ## min and max
                 xmin <- plot_df %>% pull(prob) %>% quantile(., probs = .001) - 0.1
                 xmax <- plot_df %>% pull(prob) %>% quantile(., probs = .999) + 0.1

                 ## -----------------
                 ## lines
                 ## -----------------

                 plot_df <- plot_df %>%
                     ## group by question
                     group_by(cat, var) %>%
                     ## get other quantiles
                     mutate(q025 = quantile(prob, 0.025),
                            q25 = quantile(prob, 0.25),
                            q75 = quantile(prob, 0.75),
                            q975 = quantile(prob, 0.975)) %>%
                     ## drop probs
                     select(-prob) %>%
                     ## filter to distinct values
                     distinct() %>%
                     ## ungroup
                     ungroup() %>%
                     ## get highest poststratified median
                     arrange(q50) %>%
                     ## reverse factor levels so plots correctly
                     mutate(cat_name = fct_rev(cat_name))

                 ## color by question category
                 g <- ggplot(plot_df) +
                     geom_segment(aes(y = cat_name, yend = cat_name, x = q025,
                                      xend = q975), colour = blue_hi,
                                  size = .5, show.legend = FALSE) +
                     geom_segment(aes(y = cat_name, yend = cat_name, x = q25,
                                      xend = q75), colour = blue_hi,
                                  size = 1.5, show.legend = FALSE) +
                     geom_point(aes(y = cat_name, x = q50),
                                size = 2.8, fill = "white", colour = "white") +
                     geom_point(aes(y = cat_name, x = q50),
                                fill = "white", size = 2.8,
                                colour = blue_hi, shape = 21) +
                     facet_wrap(~ var, ncol = 1, scales = "free_y",
                                labeller = labeller(var = facet_labels)) +
                     scale_y_discrete(position = "right") +
                     scale_x_continuous(breaks = seq(0, 1, 0.05),
                                        labels = percent_format(accuracy = 1L)) +
                     coord_cartesian(clip = "off",
                                     expand = TRUE) +
                     labs(x = "Poststratified percentage of students selecting factor",
                          y = NULL,
                          title = ptitle,
                          subtitle = NULL) +
                     theme_bw() +
                     theme(panel.grid.minor.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.text.x = element_text(hjust = 0),
                           text = element_text(size = 12),
                           legend.justification = c(0,1),
                           legend.position = c(0.01,0.99))

                 ## save plot
                 ggsave(filename = paste0("ranked_posterior_groups_",
                                          .x,
                                          "_lines.",
                                          pl_device),
                        plot = g,
                        device = pl_device,
                        path = pl_path,
                        width = pl_width,
                        height = pl_height,
                        scale = pl_scale,
                        units = pl_units,
                        dpi = pl_dpi)

                 ## save data to help with paper
                 write_csv(plot_df,
                           file = file.path(dat_dir,
                                            paste0("ranked_posterior_groups_",
                                                   .x,
                                                   "_lines.csv")))

                 ## return plot
                 g
             }) %>%
    setNames(outcomes)

## patchwork plots
pp_1 <- (plots[["q5_2q4"]] + theme(axis.title.x = element_blank())) /
    (plots[["q12_2"]] + theme(axis.text.y = element_blank(),
                              axis.title.x = element_blank()) |
     plots[["q12_4"]] + theme(axis.title.x = element_blank()))  +
    plot_annotation(tag_levels = list(paste0("(", LETTERS[1:3], ")")))

pp_2 <- (plots[["q6_4"]] + theme(axis.text.y = element_blank(),
                                 axis.title.x = element_blank()) |
         plots[["q7_1"]] + theme(axis.title.x = element_blank())) /
    (plots[["q11_4q10"]] + theme(axis.text.y = element_blank(),
                                 axis.title.x = element_blank()) |
     plots[["q11_2q10"]] + theme(axis.title.x = element_blank())) +
    plot_annotation(tag_levels = list(paste0("(", LETTERS[1:4], ")")))

ggsave(filename = paste0("ranked_posterior_groups_lines_combined_1.",
                         pl_device),
       plot = pp_1,
       device = pl_device,
       path = pl_path,
       width = 7.5,
       height = 9,
       scale = pl_scale,
       units = pl_units,
       dpi = pl_dpi)

ggsave(filename = paste0("ranked_posterior_groups_lines_combined_2.",
                         pl_device),
       plot = pp_2,
       device = pl_device,
       path = pl_path,
       width = 7.5,
       height = 9,
       scale = pl_scale,
       units = pl_units,
       dpi = pl_dpi)

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
