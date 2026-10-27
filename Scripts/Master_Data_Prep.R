
# Master Script for sourcing all scripts that compile and clean data (scripts 1 - 8) prior to analysis (script 9)

source("Scripts/01-census_prep.R")

source("Scripts/02-density_prep.R")

source("Scripts/03-growthrate_prep.R")

source("Scripts/04-mother_birthyear_prep.R")

source("Scripts/05-tree_conecount_prep.R")

source("Scripts/06-cone_cache_prep.R")

source("Scripts/07-final_merge.R")

source("Scripts/08-summary_figures.R")

