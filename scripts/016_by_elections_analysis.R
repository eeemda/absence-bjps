# Metadata ####################################################################
# Name: 018_by_elections_analysis.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Run analysis on by-elections cyle

# Load packages ###############################################################

library(data.table)
library(fixest)
library(dplyr)
library(stringr)
library(broom)
library(ggplot2)
library(patchwork)
library(dotwhisker)
library(modelsummary)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
library(here)

# Loading data ################################################################

source(here("scripts/functions.R"))

df_src <- readRDS(here("data/clean/absence_src_clean.rds"))

eventStudy(
    government = "government",
    event_study = TRUE,
    by_election = TRUE,
    df = df_src
)

ggsave(here("data/output/figures/anyabsencecyclegovernmentbyelection.pdf"))

eventStudy(
    government = "private",
    event_study = TRUE,
    by_election = TRUE,
    df = df_src
)

ggsave(here("data/output/figures/anyabsencecycleprivatebyelection.pdf"))
