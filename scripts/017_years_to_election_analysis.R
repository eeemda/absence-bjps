# Metadata ####################################################################
# Name: 018_years_to_election_analysis.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Creates appendix tables that instead of using dummies for relative
# years to election, uses the years to election as the key independent variable

# Packages ###################################################################

library(tibble)
library(fixest)
library(modelsummary)
library(tinytable)
library(here)

# Load data ###################################################################

df_src <- readRDS(here("data/clean/absence_src_clean.rds"))

# Table: Any Absence by years to election in government schools ###############

# Dummy for absent
formula <- as.formula(
    absent ~ years_to_election + teachers + rural + lag_absent
)

# No FEs
lm_no_fe <- feols(
    formula,
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# Year FEs
lm_year_fe <- feols(
    formula,
    fixef = "year",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# School FEs
lm_school_fe <- feols(
    formula,
    fixef = "school_code",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# School and Year FEs
lm_year_school_fe <- feols(
    formula,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# No Lagged DV
lm_year_school_fe_no_lag <- feols(
    absent ~ years_to_election + teachers + rural,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# Log Number of Absences
formula <- as.formula(
    ihs_average_absence ~ years_to_election + teachers + rural + lag_absent
)

# No FEs
log_no_fe <- feols(
    formula,
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# Year FEs
log_year_fe <- feols(
    formula,
    fixef = "year",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# School FEs
log_school_fe <- feols(
    formula,
    fixef = "school_code",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# School and Year FEs
log_year_school_fe <- feols(
    formula,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# No Lagged DV
log_year_school_fe_no_lag <- feols(
    absent ~ years_to_election + teachers + rural,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 0)
)

# Getting footer material #####################################################

n_school_fe <- format(
    lm_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_year_school_fe <- format(
    lm_year_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_year_school_fe_no_lag <- format(
    lm_year_school_fe_no_lag[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_log_school_fe <- format(
    log_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_log_year_school_fe <- format(
    log_year_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_log_year_school_fe_no_lag <- format(
    log_year_school_fe_no_lag[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

# Producing table of years to election ########################################

models_list <- list(
    lm_no_fe, lm_year_fe, lm_school_fe, lm_year_school_fe,
    lm_year_school_fe_no_lag, log_no_fe, log_year_fe, log_school_fe,
    log_year_school_fe, log_year_school_fe_no_lag
)

f <- function(x) format(round(x, 3), big.mark = ",")
gof <- list(
    list("raw" = "fe: year", "clean" = "Year FE", "fmt" = f),
    list("raw" = "fe: school_code", "clean" = "School FE", "fmt" = f),
    list("raw" = "nobs", "clean" = "Observations", "fmt" = f)
)

rows <- tribble(
    ~term, ~nofe, ~yearfe, ~schoolfe, ~yearschoolfe, ~nolag, ~nofeteachers, ~yearfeteachers, ~schoolfeteachers, ~yearschoolfeteachers, ~nolagteachers,
    "Year FE", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes",
    "School FE", "No", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes",
    "Lagged DV", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "No",
    "Number of Schools", n_school_fe, n_school_fe, n_school_fe,
    n_year_school_fe, n_year_school_fe_no_lag, n_log_school_fe, n_log_school_fe,
    n_log_school_fe, n_log_year_school_fe, n_log_year_school_fe_no_lag
)
attr(rows, "position") <- c(3, 4, 5, 6, 7)
table_note <- "\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses. The dependent variable in columns 1-5 is a dummy variable that takes the value of one if the school reports any teacher absenteeism in that year and the log number of absences per teacher in columns 6-10.  Each specification includes controls for the number of teachers in each school, a dummy for whether the school is in a rural area. Columns 1-4 and 6-9 include a lagged dependent variable.  ``Years to Election'' is a variable equal to the number of years that the school is from an election."

table_years_to_election_gov <- modelsummary(
    models_list,
    title = "Absence by Years to Election in Government Schools\\label{appendixtable:yearstoelectiongovernment}",
    coef_map = c("years_to_election" = "Years to Election"),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    gof_map = gof,
    add_rows = rows,
    notes = table_note,
    output = "tinytable",
    escape = FALSE
    ) %>%
    group_tt(
        j = list("Absence" = 2:6, "Log Average Absence" = 7:11)
    ) %>%
    style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
    save_tt(
        here("data/output/tables/anyabsencegovernmentyears.tex"),
        overwrite = TRUE
    )

 # Table: Any Absence by years to election in private schools ###############

# Dummy for absent
formula <- as.formula(
    absent ~ years_to_election + teachers + rural + lag_absent
)

# No FEs
lm_no_fe <- feols(
    formula,
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# Year FEs
lm_year_fe <- feols(
    formula,
    fixef = "year",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# School FEs
lm_school_fe <- feols(
    formula,
    fixef = "school_code",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# School and Year FEs
lm_year_school_fe <- feols(
    formula,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# No Lagged DV
lm_year_school_fe_no_lag <- feols(
    absent ~ years_to_election + teachers + rural,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# Log Number of Absences
formula <- as.formula(
    ihs_average_absence ~ years_to_election + teachers + rural + lag_absent
)

# No FEs
log_no_fe <- feols(
    formula,
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# Year FEs
log_year_fe <- feols(
    formula,
    fixef = "year",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# School FEs
log_school_fe <- feols(
    formula,
    fixef = "school_code",
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# School and Year FEs
log_year_school_fe <- feols(
    formula,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# No Lagged DV
log_year_school_fe_no_lag <- feols(
    absent ~ years_to_election + teachers + rural,
    fixef = c("year", "school_code"),
    cluster = c("unique_constituency_code", "year"),
    data = subset(df_src, private == 1)
)

# Getting footer material #####################################################

n_school_fe <- format(
    lm_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_year_school_fe <- format(
    lm_year_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_year_school_fe_no_lag <- format(
    lm_year_school_fe_no_lag[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_log_school_fe <- format(
    log_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_log_year_school_fe <- format(
    log_year_school_fe[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

n_log_year_school_fe_no_lag <- format(
    log_year_school_fe_no_lag[["fixef_sizes"]][["school_code"]],
    big.mark = ",",
    trim = TRUE
)

# Producing table of years to election ########################################

models_list <- list(
    lm_no_fe, lm_year_fe, lm_school_fe, lm_year_school_fe,
    lm_year_school_fe_no_lag, log_no_fe, log_year_fe, log_school_fe,
    log_year_school_fe, log_year_school_fe_no_lag
)

f <- function(x) format(round(x, 3), big.mark = ",")
gof <- list(
    list("raw" = "fe: year", "clean" = "Year FE", "fmt" = f),
    list("raw" = "fe: school_code", "clean" = "School FE", "fmt" = f),
    list("raw" = "nobs", "clean" = "Observations", "fmt" = f)
)

rows <- tribble(
    ~term, ~nofe, ~yearfe, ~schoolfe, ~yearschoolfe, ~nolag, ~nofeteachers, ~yearfeteachers, ~schoolfeteachers, ~yearschoolfeteachers, ~nolagteachers,
    "Year FE", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes",
    "School FE", "No", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes",
    "Lagged DV", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "No",
    "Number of Schools", n_school_fe, n_school_fe, n_school_fe,
    n_year_school_fe, n_year_school_fe_no_lag, n_log_school_fe, n_log_school_fe,
    n_log_school_fe, n_log_year_school_fe, n_log_year_school_fe_no_lag
)
attr(rows, "position") <- c(3, 4, 5, 6, 7)
table_note <- "\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses. The dependent variable in columns 1-5 is a dummy variable that takes the value of one if the school reports any teacher absenteeism in that year and the log number of absences per teacher in columns 6-10.  Each specification includes controls for the number of teachers in each school, a dummy for whether the school is in a rural area. Columns 1-4 and 6-9 include a lagged dependent variable.  ``Years to Election'' is a variable equal to the number of years that the school is from an election."

table_years_to_election_private <- modelsummary(
    models_list,
    title = "Absence by Years to Election in Private Schools\\label{appendixtable:yearstoelectionprivate}",
    coef_map = c("years_to_election" = "Years to Election"),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    gof_map = gof,
    add_rows = rows,
    notes = table_note,
    output = "tinytable",
    escape = FALSE
    ) %>%
    group_tt(
        j = list("Absence" = 2:6, "Log Average Absence" = 7:11)
    ) %>%
    style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
    save_tt(
        here("data/output/tables/anyabsenceprivateyears.tex"),
        overwrite = TRUE
    )