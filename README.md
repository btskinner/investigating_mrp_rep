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

### Included
The following included files are required:

1. `second_level_df.rds`

### To be downloaded
The following IPEDS data files are required:

1. **`HD2019.zip`**: `hd2019.csv`
1. **`EFFY2019.zip`**: `effy2019_rv.csv`
1. **`SFA1819.zip`**: `sfa1819.csv`
1. **`GR2019.zip`**: `gr2019.csv`
1. **`C2019_C.zip`**: `c2019_c_rv.csv`

The second file in each item can be unzipped from the first, which can be
downloaded from
[https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx]() and placed
into `./data/external`





