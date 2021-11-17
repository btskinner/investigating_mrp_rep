################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] checks.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 27 March 2020
##
################################################################################

## libraries
libs <- c("haven", "tidyverse", "rstan", "xtable")
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

## functions
source(file.path(scr_dir, "functions.R"))

## colors
blue <- "#0072B2"
orange <- "#E69F00"
red <- "#D55E00"
yellow <- "#F0E442"

## plot settings
pl_device <- "pdf"
pl_path <- fig_dir
pl_width <- 7.5
pl_height <- 8
pl_scale <- 1.45
pl_units <- "in"
pl_dpi <- "retina"

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

pars_list <- map(full_fit_list,
                 ~ extract(., pars = c("p_hat")))

## -----------
## test: q3_2
## -----------

questions <- names(data_list)

df <- map(questions,
          ~ {
              clicks <- data_list[[.x]][["clicks"]] %>% sum
              views = data_list[[.x]][["views"]] %>% sum
              pp <- inv_logit(pars_list[[.x]][[1]])
              pred <- rbinom(nrow(pp), views, rowMeans(pp))
              tibble(question = .x,
                     clicks = clicks,
                     views = views,
                     pred = pred)
          }) %>%
    bind_rows() %>%
    mutate(qlab = add_qlabs(question),
           question = str_replace(question, "q", ""),
           question = str_replace(question, "_", "."),
           question = as.numeric(question),
           qname = paste0("Question ", question),
           stat = as.integer(pred >= clicks)) %>%
    arrange(question) %>%
    mutate(question = factor(question, levels = question, labels = qname))

df_pval <- df %>%
    group_by(question) %>%
    summarize(pval = paste0("p = ", sprintf("%.2f",(round(sum(stat) / n(), 2)))))

## -----------------------------------------------
## figure for paper
## -----------------------------------------------

g <- ggplot(df, aes(x = pred, fill = "Predicted")) +
    facet_wrap(~ question, scales = "free_x", ncol = 4) +
    geom_histogram(colour = "black", bins = 30, alpha = 0.3) +
    geom_vline(aes(xintercept = clicks, colour = "Observed"), size = 1.25) +
    geom_text(data = df_pval,
              aes(label = pval), x = Inf, y = Inf, hjust = 1.2, vjust = 1.5,
            inherit.aes = FALSE) +
    scale_fill_manual(name = "", values = orange) +
    scale_colour_manual(name = "", values = blue) +
    labs(x = "Number of clicks",
         y = NULL,
         title = "Posterior predictive distributions for each choice") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.box = "horizontal",
          legend.spacing.y = unit(0, "cm"),
          legend.position = c(0.87, 0.03)) +
    guides(colour = guide_legend(order = 1),
           fill = guide_legend(order = 2))

## save plot
ggsave(filename = paste0("posterior_predictive_check_counts.", pl_device),
       plot = g,
       device = pl_device,
       path = pl_path,
       width = pl_width,
       height = pl_height,
       scale = pl_scale,
       units = pl_units,
       dpi = pl_dpi)

## -----------------------------------------------------------------------------
## make table for appendix concordance
## -----------------------------------------------------------------------------

out_mat <- df %>%
    group_by(question) %>%
    filter(row_number() == 1) %>%
    select(question, qlab) %>%
    as.matrix

## header
head <- c("\\begin{table}[!ht]",
          "\\centering",
          "\\footnotesize",
          "\\caption{Question number and name concordance}",
          "\\label{tab:concord}",
          "\\begin{tabularx}{\\linewidth}{lX}",
          "\\toprule",
          "Question number & Question name \\\\")

## primary contents
contents <- print(xtable(out_mat),
                  booktabs = TRUE,
                  sanitize.text.function = function(x){x},
                  include.colnames = FALSE,
                  include.rownames = FALSE,
                  only.contents = TRUE,
                  print.results = FALSE,
                  hline.after = c(-1, 0, nrow(out_mat)),
                  comment = FALSE)

## footer
foot <- c("\\end{tabularx}",
          "\\end{table}")

## write out table in TeX
writeLines(c(head, contents, foot), con = file.path(tab_dir, "concord_tab.tex"))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
