library(tidyverse)
library(broom)
library(ggplot2)
library(dotwhisker)
library(xtable)
library(modelsummary)
library(tinytable)
library(here)

###############################################################################
# Loading data
###############################################################################

source(here("scripts/functions.R"))
df_src <- readRDS(here::here("data/clean/absence_src_clean.rds"))

# Calculate election year mean
sink(file = here("data/output/text/electionyearmean.tex"))
cat(
    round(
        mean(
            df_src$absent[df_src$private == 0 & df_src$vs_year_2 == 1],
            na.rm = TRUE
        )
        * 100,
        0
    )
)
sink(file = NULL)

###############################################################################
# Table 2: Any Absence in an Election Year in Government Schools
###############################################################################

eventStudy(
    government = "government",
    event_study = FALSE,
    by_election = FALSE,
    df = df_src
)

###############################################################################
# Table 3: Any Absence in an Election Year in Private Schools
###############################################################################

eventStudy(
    government = "private",
    event_study = FALSE,
    by_election = FALSE,
    df = df_src
)

###############################################################################
# Figure 1: Any Absence over the Electoral Cycle in Government Schools
# Appendix Table 1
###############################################################################

eventStudy(
    government = "government",
    event_study = TRUE,
    by_election = FALSE,
    df = df_src
)

# Extracting point estimate for manuscript
formula <- as.formula(
    absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
    lag_absent + teachers
)

lm_year_school_fe_teachers <- feols(
    formula,
    cluster = c("unique_constituency_code", "year"),
    fixef = c("year", "school_code"),
    data = subset(df_src, private == 0)
)

sink(file = here("data/output/text/preelectionpointestimategovernment.tex"))
cat(round(abs(lm_year_school_fe_teachers$coefficients[[2]]) * 100, 1))
sink(file = NULL)

sink(file = here("data/output/text/postelectionpointestimategovernment.tex"))
cat(round(abs(lm_year_school_fe_teachers$coefficients[[3]]) * 100, 1))
sink(file = NULL)

###############################################################################
# Figure 2: Any Absence over the Electoral Cycle in Private Schools
# Appendix Table 2
###############################################################################

eventStudy(
    government = "private",
    event_study = TRUE,
    by_election = FALSE,
    df = df_src
)

lm_year_school_fe_teachers <- feols(
    formula,
    cluster = c("unique_constituency_code", "year"),
    fixef = c("year", "school_code"),
    data = subset(df_src, private == 1)
)

sink(file = here("data/output/text/preelectionpointestimateprivate.tex"))
cat(round(abs(lm_year_school_fe_teachers$coefficients[[2]]) * 100, 1))
sink(file = NULL)

sink(file = here("data/output/text/postelectionpointestimateprivate.tex"))
cat(round(abs(lm_year_school_fe_teachers$coefficients[[3]]) * 100, 1))
sink(file = NULL)