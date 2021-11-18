# Replication files

This repository contains scripts and instructions for replicating

> Ortagus, J., Skinner, B.T., and Tanner, M. (2021) Investigating Why
> Academically Successful Community College Students Leave College
> without a Degree

## To run

There are two options to run. After cloning the repository and adding
necessary data (see below), either:

1. Navigate to the top-level of the project directory and use the
`makefile`

``` shell
git clone https://github.com/btskinner/investigating_mrp_rep.git
cd investigating_mrp_rep
make
```
2. Run the following scripts in order from the `./scripts/r`
   directory:
   
   1. `get_packages.r`
   1. `get_data.r`
   1. `make_mrp_data.r`
   1. `fit_stan.r`
   1. `poststratify.r`
   1. `make_figures.r`
   1. `make_tables.r`
   1. `checks.r`

And then compile the tables and figures in the `docs` directory using:

``` shell
latexmk -pdf -silent tables_figures.tex
```

## Data
### Restricted (requires permission)
The following restricted data files are required:

1. `analysis_data.dta`
1. `poststrat_counts.dta`
1. `full_no_drop_demographics_data.dta`
1. `main_crosswalk.dta`

To access these files, see the included file, `ACCESSING_RESTRICTED_DATA.md`.

### Created
The following file is created by `get_data.R`:

1. `second_level_df.rds`

A number of intermediate files from IPEDS and the BLS will be
downloaded in the process.




