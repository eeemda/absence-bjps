# Metadata ####################################################################
# Name: 000_run_file.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Sources all the individuals scripts to reproduce results for
# "Absence: Electoral Cycles and Teacher Absenteeism in India", conditionally
# accepted at the British Journal of Political Science

library(here)

source(here("scripts/001_clean_elections.R"))
source(here("scripts/002_clean_ihds.R"))
source(here("scripts/003_merge_ac_district.R"))
source(here("scripts/004_dise_clean.R"))
source(here("scripts/005_summary_statistics_tables.R"))
source(here("scripts/006_analyse_ihds.R"))
source(here("scripts/007_analyse_dise.R"))
source(here("scripts/008_analyse_test_scores.R"))
source(here("scripts/009_match_rate_table.R"))
source(here("scripts/010_mechanisms.R"))
source(here("scripts/011_match_differences.R"))
source(here("scripts/012_absence_cost.R"))
source(here("scripts/013_figure1.R"))
source(here("scripts/014_chaudhury_comparison.R"))
source(here("scripts/015_mla_profession_analysis.R"))
source(here("scripts/016_by_elections_analysis.R"))
source(here("017_years_to_election_analysis.R"))

# I comment this out as it takes a couple of hours to run this script
# source(here("018_bounds_analysis.R"))