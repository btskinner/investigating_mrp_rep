################################################################################
#
# [ PROJ ] Helios survey responses with MRP
# [ FILE ] makefile
# [ AUTH ] Benjamin Skinner (@btskinner)
# [ CITE ]
#
#  Ortagus, J., Skinner, B.T., and Tanner, M. (2021). Investigating
#  Why Academically Successful Community College Students Leave
#  College without a Degree
#
################################################################################

# --- directories --------------------------------

# get root of project (current directory)
DIR_ROOT := $(shell pwd)

# data 
DIR_DATA := data
DIR_CLEAN := $(DIR_DATA)/clean

# scripts
DIR_SCRIPT := scripts
DIR_R      := $(DIR_SCRIPT)/r
DIR_STAN   := $(DIR_SCRIPT)/stan

# model output
DIR_EST := $(DIR_DATA)/estimates

# docs/table/figures
DIR_DOC := docs
DIR_TAB := tables
DIR_FIG := figures

# --- wildcard files -----------------------------

# macros
MACROS := $(DIR_R)/macros.R

# labels
LABELS := $(DIR_R)/labels.R

# functions
FUNCTIONS := $(DIR_R)/functions.R

# Stan file
STAN := $(DIR_STAN)/mrp.stan

# data
DAT_RFITS := $(DIR_EST)/full_fit_list.rds
DAT_RPRED := $(DIR_EST)/predictions.rds
DAT_RSTAN := $(DIR_CLEAN)/stan_dat_q3_1.Rdump
DAT_STATA := $(DIR_CLEAN)/analysis_data.dta
DAT_SECLV := $(DIR_CLEAN)/second_level_df.rds

# figures
FIGS := $(DIR_FIG)/ranked_posterior_overall_lines.pdf

# tables
TABS := $(DIR_TAB)/demo_tab.tex

# checks
CHKS := $(DIR_FIG)/posterior_predictive_check_counts.pdf

# docs
DOCS := $(DIR_DOC)/tables_figures.pdf

# TeX auxilary
TEX_AUX := $(addprefix $(DIR_DOC)/, *.aux *.bbl *.bcf *.blg \
	*.fdb_latexmk *.fff *.fls *.lof *.log *.lot *.out *.run.xml *.ttt)

# --- build targets ------------------------------

.PHONY: all
all: setup data analysis output

# .PHONY: data			
data_get: $(DAT_SECLV)
data_stan: $(DAT_RSTAN)
data: data_get data_stan

.PHONY: analysis_fit analysis_poststrat checks analysis
analysis_fit: $(DAT_RFITS)
analysis_postrat: $(DAT_RPRED)
checks: $(CHKS)
analysis: analysis_fit analysis_poststrat checks

.PHONY: figures tables docs output
figures: $(FIGS)
tables: $(TABS)
docs: $(DOCS)
output: figures tables docs

# --- setup --------------------------------------

.PHONY: setup
setup: $(DIR_R)/get_packages.R
	@echo "\n==> Getting required R packages\n"
	@Rscript $<
	@echo ""

# --- data ---------------------------------------

$(DAT_SECLV): $(DIR_R)/get_data.R $(MACROS)
	@echo "\n==> Munging required external data\n"
	@mkdir -p $(DIR_CLEAN)
	@Rscript $< $(DIR_ROOT)
	@echo ""

$(DAT_RSTAN): $(DIR_R)/make_mrp_data.R $(DAT_STATA) $(MACROS)
	@echo "\n==> Setting up data for Stan: R\n"
	@Rscript $< $(DIR_ROOT)
	@echo ""

# --- analysis -----------------------------------

$(DAT_RFITS): $(DIR_R)/fit_stan.R $(DAT_RSTAN) $(MACROS) $(STAN)
	@echo "\n==> Fitting Stan model\n"
	@Rscript $< $(DIR_ROOT)

$(DAT_RPRED): $(DIR_R)/poststratify.R $(DAT_RFITS) $(MACROS) $(FUNCTIONS)
	@echo "\n==> Poststratifying\n"
	@Rscript $< $(DIR_ROOT)
	@echo ""

# --- figures ------------------------------------

$(FIGS): $(DIR_R)/make_figures.R $(DAT_RPRED) $(MACROS) $(LABELS)
	@echo "\n==> Making figures\n"
	@mkdir -p figures
	@Rscript $< $(DIR_ROOT)
	@echo ""

# --- tables -------------------------------------

$(TABS): $(DIR_R)/make_tables.R $(DAT_RPRED) $(MACROS) $(LABELS)
	@echo "\n==> Making tables\n"
	@mkdir -p tables
	@Rscript $< $(DIR_ROOT)
	@echo ""

# --- checks -------------------------------------

$(CHKS): $(DIR_R)/checks.R $(DAT_RPRED) $(MACROS) $(LABELS)
	@echo "\n==> Performing modeling checks\n"
	@mkdir -p tables
	@mkdir -p figures
	@Rscript $< $(DIR_ROOT)
	@echo ""

# --- docs ---------------------------------------

$(DOCS): $(DIR_DOC)/tables_figures.tex $(TABS) $(FIGS) $(MACROS) $(LABELS)
	@echo "\n==> Compiling TeX files to PDF\n"
	latexmk -pdf -silent -cd $<
	$(RM) $(TEX_AUX)
	@echo ""

# --- Clean --------------------------------------

clean:
	@echo "\n==> Returning repo to initial state\n"	
	$(RM) -r $(DIR_EST)/* $(DIR_CLEAN)/*
	$(RM) $(DIR_DOC)/tables_figures.pdf $(DIR_STAN)/mrp
	@echo "Clean!\n"
