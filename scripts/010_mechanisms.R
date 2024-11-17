# Metadata ####################################################################
# Name: 010_mechanisms.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purposes: Runs analysis on the mechanisms of teacher absenteeism including
# 1. Increase bureaucratic effort
# 2. Increased parental effort
# 3. Electoral competition
# Date created: 2021/05/13

# Loading packages ############################################################

library(fixest)
library(dplyr)
library(modelsummary)
library(rio)
library(here)

# Loading data ###############################################################

df_src <- readRDS(here("data/clean/absence_src_clean.rds"))

source(here("scripts/functions.R"))

# Theoretical channels ########################################################

formula_base <- as.formula(
    c(administrative_visits, smc_meetings) ~ vs_year_0 + vs_year_1 +
    vs_year_3 + vs_year_4 + rural + lag_absent + teachers 
)

models_channels <- feols(
    formula_base,
    data = subset(df_src, private == 0),
    cluster = c("unique_constituency_code", "year"),
    fixef = "year"
)

plot_visits <- tidyCyclePlots(
    model = models_channels[[1]],
    interaction = FALSE,
    log_outcome = FALSE
)

plot_visits <- plot_visits + labs(x = "Number of Visits")

plot_smc <- tidyCyclePlots(
    model = models_channels[[2]],
    interaction = FALSE,
    log_outcome = FALSE
)

plot_smc <- plot_smc + labs(x = "Number of SMC Meetings")

plots <- list(plot_visits, plot_smc)

plot_export <- wrap_plots(plots, ncol = 1, axes = "collect") +
    plot_layout(axes = "collect_x") +
    plot_annotation(tag_levels = "A")

ggsave(
    here("data/output/figures/alternativechannels.pdf"),
    height = 4.5,
    width = 6.5,
    units = "in",
    bg = "transparent"
)

# Full results tables #########################################################

event_study_channels(outcome = "administrative_visits", df = df_src)
event_study_channels(outcome = "smc_meetings", df = df_src)

# Mechanisms results ##########################################################

# Margin of victory
lm_margins <- feols(
    c(absent, ihs_average_absence) ~ vs_year_0 * analytic_margin +
    vs_year_1 * analytic_margin + vs_year_3 * analytic_margin +
    vs_year_4 * analytic_margin + teachers + rural,
    data = subset(df_src, private == 0),
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year")
)

n_schools_margins <- format(
    lm_margins[[1]][["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_schools_margins_log <- format(
    lm_margins[[2]][["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

# Effective Number of Parties:
lm_enop <- feols(
    c(absent, ihs_average_absence) ~ vs_year_0 * analytic_enop +
    vs_year_1 * analytic_enop + vs_year_3 * analytic_enop +
    vs_year_4 * analytic_enop + teachers + rural,
    data = subset(df_src, private == 0),
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year")
)

n_schools_enop <- format(
    lm_enop[[1]][["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_schools_enop_log <- format(
    lm_enop[[2]][["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

# Aligned with Ruling Party:
lm_alignment <- feols(
    c(absent, ihs_average_absence) ~ vs_year_0 * analytic_alignment +
    vs_year_1 * analytic_alignment + vs_year_3 * analytic_alignment +
    vs_year_4 * analytic_alignment + teachers + rural,
    data = subset(df_src, private == 0),
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year")
)

n_schools_alignment <- format(
    lm_alignment[[1]][["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_schools_alignment_log <- format(
    lm_alignment[[2]][["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

# Exporting models for mechanisms #############################################

models_list <- list(
    lm_enop[[1]], lm_margins[[1]], lm_alignment[[1]], lm_enop[[2]],
    lm_margins[[2]], lm_alignment[[2]]
)

f <- function(x) format(round(x, 3), big.mark = ",")
    gof <- list(
        list("raw" = "fe: year", "clean" = "Year FE", "fmt" = f),
        list("raw" = "fe: school_code", "clean" = "School FE", "fmt" = f),
        list("raw" = "nobs", "clean" = "Observations", "fmt" = f)
    )

rows <- tribble(
    ~term, ~enop, ~margins, ~alignment, ~enop_log, ~margins_log, ~alignment_log,
    "Number of Schools", n_schools_enop, n_schools_margins, n_schools_alignment,
    n_schools_enop_log, n_schools_margins_log, n_schools_alignment_log
)

# Getting Reference Category Mean:
mean_absence <- round(
    mean(
        df_src$absent[df_src$vs_year_2 == 0 & df_src$private == 0],
        na.rm = TRUE
    ),
    3
)

sink(file = here("data/output/text/meanabsence.tex"))
cat(mean_absence)
sink(file = NULL)

table_note <- "\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses.  All columns include controls for the number of teachers in the school, whether the school is in a rural area, and school and year fixed effects. Column two interacts the year dummies with the margin of victory in the reference year election.  Column one interacts the year dummies with the effective number of parties in the reference year election.  Column three interacts the year dummies with whether the school is a constituency government by an MLA that is also a member of the ruling coalition at the state level."

coef_map <- c(
    "vs_year_0" = "2 or More Years Before Election",
    "vs_year_1" = "1 Year Before Election",
    "vs_year_3" = "1 Year After Election",
    "vs_year_4" = "2 or More Years After Election",
    "analytic_enop" = "Effective Number of Parties",
    "analytic_margin" = "Margin of Victory",
    "analytic_alignment" = "In Governing Coalition",
    "vs_year_0:analytic_enop" = "-2 Years from Election x ENOP",
    "analytic_enop:vs_year_1" = "-1 Year from Election x ENOP",
    "analytic_enop:vs_year_3" = "1 Year from Election x ENOP",
    "analytic_enop:vs_year_4" = "2 Years from Election x ENOP",
    "vs_year_0:analytic_margin" = "-2 Years from Election x Margin",
    "analytic_margin:vs_year_1" = "-1 Year from Election x Margin",
    "analytic_margin:vs_year_3" = "1 Year from Election x Margin",
    "analytic_margin:vs_year_4" = "2 Years from Election x Margin",
    "vs_year_0:analytic_alignment" = "-2 Years from Election x Coalition",
    "analytic_alignment:vs_year_1" = "-1 Year from Election x Coalition",
    "analytic_alignment:vs_year_3" = "1 Year from Election x Coalition",
    "analytic_alignment:vs_year_4" = "2 Years from Election x Coalition"
)

mechanisms_table <- modelsummary(
    models_list,
    title = "Mechanisms of Reduced Absenteeism\\label{table:channels}",
    coef_map = coef_map,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    gof_map = gof,
    add_rows = rows,
    notes = table_note,
    output = "tinytable",
    escape = FALSE
    ) %>%
    group_tt(j = list("Absent" = 2:4, "Average Number of Absences" = 5:7)) %>%
    style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
    save_tt(here("data/output/tables/mechanisms.tex"), overwrite = TRUE)
