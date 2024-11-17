# Load Packages ###############################################################

library(lfe)
library(fixest)
library(sandwich)
library(dotwhisker)
library(dplyr)
library(stringr)
library(broom)
library(ggplot2)
library(patchwork)
library(modelsummary)
library(tinytable)
library(here)

# Load data and functions #####################################################

load(file = here::here("data/clean/ihds_schools_elections.RData"))
load(file = here::here("data/clean/ihds_staff.RData"))
load(file = here::here("data/clean/ihds_student.RData"))
source(here::here("scripts/functions.R"))

# Allow for body fonts when exporting to latex with modelsummary, otherwise
# wraps in \num{} command and uses mathematical fonts
options(modelsummary_format_numeric_latex = "plain")

path_tables <- "data/output/tables"
path_figures <- "data/output/figures"

page_width = 6.5
page_height = 9

# Merging election dates with school staff data ###############################

df_ihds_clean <- left_join(
	df_ihds_staff_clean, df_ihds_school_clean,
	by = c("school_id")
	) %>%
	select(-ends_with(".y")) %>%
	rename_with(~str_remove(., ".x")) %>%
	rename_with(~str_remove(., "_no_by"))

# Mean level of absenteeism in government and private schools #################

sink(file = here("data/output/text/meanabsenceihds.tex"))
cat(round(mean(df_ihds_clean$absent_school[df_ihds_clean$government_school == 1], na.rm = TRUE) * 100, 0))
sink(file = NULL)

# Linear model of absence and type of school ##################################

# Models of Absence:
lm_absence <- felm(absent_school ~ government_school + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school, data = df_ihds_clean)
cov <- vcovHC(lm_absence, type = "HC1")
se_lm_absence <- sqrt(diag(cov))

lm_absence_fe <- felm(absent_school ~ government_school + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school | id_psu, data = df_ihds_clean)
cov <- vcovHC(lm_absence_fe, type = "HC1")
se_lm_absence_fe <- sqrt(diag(cov))

# Models of absence for official duty:
lm_absent_duty <- felm(absent_work ~ government_school + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school, data = df_ihds_clean)
cov <- vcovHC(lm_absent_duty, type = "HC1")
se_lm_absent_duty <- sqrt(diag(cov))

lm_absent_duty_fe <- felm(absent_work ~ government_school + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school | id_psu, data = df_ihds_clean)
cov <- vcovHC(lm_absent_duty_fe, type = "HC1")
se_lm_absent_duty_fe <- sqrt(diag(cov))

# Models of Absence from survey:
lm_absent_interview <- felm(absent_interview ~ government_school + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school, data = subset(df_ihds_clean, absent_school == 0))
cov <- vcovHC(lm_absent_interview, type = "HC1")
se_lm_absent_interview <- sqrt(diag(cov))

lm_absent_interview_fe <- felm(absent_interview ~ government_school + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school | id_psu, data = subset(df_ihds_clean, absent_school == 0))
cov <- vcovHC(lm_absent_interview_fe, type = "HC1")
se_lm_absent_interview_fe <- sqrt(diag(cov))

models_list <- list(lm_absence, lm_absence_fe, lm_absent_duty,
	lm_absent_duty_fe, lm_absent_interview, lm_absent_interview_fe)

# Formatting Table
format_n <- function(x) format(round(x, 0), big.mark = ",")
gof <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = format_n),
    list("raw" = "fe", "clean" = "Village Fixed Effects", "fmt" = format_n)
)

rows <- tribble(
	~term, ~absence, ~absencefe, ~absentduty, ~absentdutyfe, ~absentinterview,
	~absentinterviewfe,
	"Village Fixed Effects", "N", "Y", "N", "Y", "N", "Y"
)
attr(rows, "position") <- (3)

vcov_matrix <- list(se_lm_absence, se_lm_absence_fe, se_lm_absent_duty,
		se_lm_absent_duty_fe, se_lm_absent_interview,
		se_lm_absent_interview_fe)

table_ihds_absence_government <- modelsummary(
	models_list,
	title = "Absence by School Type\\label{table:absenceschooltype}",
	coef_map = c(government_school = "Government School"),
	stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    vcov = vcov_matrix,
    gof_map = gof,
    add_rows = rows,
    notes = "\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Robust standard errors are in parentheses. Linear models of the effects of working in a government school on either absence from school on the day of the interview, absent from the school for official government-sanctioned work, or absent from the interview conditional on being present at the school on the day of the interview.  All models control for gender, age, religion, and caste, while even-numbered columns also include village-fixed effects.",
    escape = FALSE,
    output = "tinytable"
) %>%
	group_tt(
		j = list(
			"Absent" = 2:3, "On Official Duty" = 4:5,
			"Absent from Interview" = 6:7
		)
	) %>%
	style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
	save_tt(
		paste0(path_tables, "/ihdsabsencegovernment.tex"),
		overwrite = TRUE
	)

# Linear model of absenteeism in election year ################################

# No fixed effects:
lm_absence <- felm(
	absent_school ~ election_year + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school,
	data = subset(df_ihds_clean, government_school == 1)
)
cov <- vcovHC(lm_absence, type = "HC1")
se_lm_absence <- sqrt(diag(cov))

lm_absence_work <- felm(absent_work ~ election_year + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school, data = subset(df_ihds_clean, government_school == 1))
cov <- vcovHC(lm_absence, type = "HC1")
se_lm_absence_work <- sqrt(diag(cov))

lm_absence_private <- felm(absent_school ~ election_year + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school, data = subset(df_ihds_clean, government_school == 0))
cov <- vcovHC(lm_absence, type = "HC1")
se_lm_absence_private <- sqrt(diag(cov))

lm_absence_work_private <- felm(absent_work ~ election_year + male + age + muslim + other_religion + upper_caste + sc_st + other_caste + distance_school, data = subset(df_ihds_clean, government_school == 0))
cov <- vcovHC(lm_absence, type = "HC1")
se_lm_absence_work_private <- sqrt(diag(cov))

models_list <- list(
	lm_absence, lm_absence_work, lm_absence_private, lm_absence_work_private
)

# Formatting table
format_n <- function(x) format(round(x, 0), big.mark = ",")
gof <- list(
	list("raw" = "nobs", clean = "N", "fmt" = format_n),
	list("raw" = "fe", "clean" = "Village Fixed Effects", fmt = format_n)
)

# Electoral cycle models by years to election #################################

lm_absence_cycle <- feols(
	absent_school ~ years_to_election + male + age +
	muslim + other_religion + upper_caste + sc_st + other_caste +
	distance_school,
	data = subset(df_ihds_clean, government_school == 1),
	cluster = "state_district"
)

election_mean_absence <- round(lm_absence_cycle$coefficients[[1]], 2)
two_year_mean_absence <- round(lm_absence_cycle$coefficients[[5]], 2)
total_two_year_absence <- (election_mean_absence + two_year_mean_absence) * 100

# Exporting election year mean
sink(file = here("data/output/text/electionmeangovernmentabsenceihds.tex"))
cat(election_mean_absence)
sink(file = NULL)

# Exporting two years from election point estimate
sink(file = here("data/output/text/twoyearelectionmeangovernmentabsenceihds.tex"))
cat(total_two_year_absence)
sink(file = NULL)

# Exporting sample size
sink(file = here::here("data/output/text/samplesizegovernmentabsenceihds.tex"))
cat(format(lm_absence_cycle$nobs, big.mark = ","))
sink(file = NULL)

lm_work_cycle <- feols(
	absent_work ~ years_to_election + male + age + muslim +
	other_religion + upper_caste + sc_st + other_caste + distance_school,
	data = subset(df_ihds_clean, government_school == 1 & absent_school == 1),
	cluster = "state_district"
)

# Exporting election year mean
sink(file = here::here("data/output/text/electionmeangovernmentdutyihds.tex"))
cat(
	round(
		mean(df_ihds_clean$absent_work[df_ihds_clean$government_school == 1 &
			df_ihds_clean$years_to_election == 0], na.rm = TRUE), 2
	)
)
sink(file = NULL)

election_mean_duty <- round(
	mean(df_ihds_clean$absent_work[df_ihds_clean$government_school == 1 & df_ihds_clean$years_to_election == 0], na.rm = TRUE), 2
)

# Formatting table for export
models_list <- list(lm_absence_cycle, lm_work_cycle)

# Formatting footer
format_n <- function(x) format(round(x, 0), big.mark = ",")
gof <- list(list("raw" = "nobs", "clean" = "N", "fmt" = format_n))

rows <- tribble(
	~term, ~absencecycle, ~workcycle,
	"Election Year Mean", as.character(election_mean_absence), as.character(election_mean_duty)
)

# Putting table together with modelsummary
table_ihds_absence_cycle_government <- modelsummary(
	models_list,
	title = "Absence Over Electoral Cycle in Government Schools Using IHDS Data\\label{table:ihdsabsencecyclegovernment}",
	coef_map = c(
		"years_to_election-2" = "2 or More Years Before Election",
		"years_to_election-1" = "1 Year Before Election",
		"years_to_election1" = "1 Year After Election",
		"years_to_election2" = "2 or More Years After Election",
		"male" = "Male",
		"age" = "Age",
		"muslim" = "Muslim",
		"other_religion" = "Other Religion",
		"upper_caste" = "Upper Caste",
		"sc_st" = "SC/ST",
		"other_caste" = "Other Caste",
		"distance_school" = "Distance from School"
	),
	stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    gof_map = gof,
    add_rows = rows,
    notes = "\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Robust standard errors in parentheses. Linear models of the probability of a teacher being absent from a government school the day IHDS surveyors surveyed the school, as well as the probability a teacher was absent on official duty as outlined in Equation \\ref{equation:businesscycle}.  The dependent variable is either a dummy variable that takes the value of one if the teacher was absent from the school on the day of the survey or, conditional on being absent, whether they were absent on official duty on the day of the survey.",
    escape = FALSE,
    output = "tinytable"
	) %>%
	group_tt(j = list("Absent" = 2, "On Official Duty" = 3)) %>%
	style_tt(i = -1:nrow(.), fontsize = 0.7)

table_ihds_absence_cycle_government %>%
	save_tt(
		paste0(path_tables, "/ihdsabsencecyclegovernment.tex"),
		overwrite = TRUE
	)

# Exporting sample size
sink(file = here::here("data/output/text/samplesizegovernmentdutyihds.tex"))
cat(format(lm_work_cycle$nobs, big.mark = ","))
sink(file = NULL)

# Electoral cycle for Private Schools by year #################################

# Absence Model
lm_absence_cycle_private <- feols(
	absent_school ~ years_to_election + male +
	age + muslim + other_religion + upper_caste + sc_st + other_caste +
	distance_school,
	data = subset(df_ihds_clean, government_school == 0),
	cluster = "state_district"
)

# Exporting election year mean
sink(file = here::here("data/output/text/electionmeanprivateabsenceihds.tex"))
cat(
	round(
		mean(df_ihds_clean$absent_school[df_ihds_clean$government_school == 0 &
			df_ihds_clean$years_to_election == 0], na.rm = TRUE), 2
	)
)
sink(file = NULL)

election_mean_absence <- round(
		mean(df_ihds_clean$absent_school[df_ihds_clean$government_school == 0 &
			df_ihds_clean$years_to_election == 0], na.rm = TRUE), 2
	)

# Exporting sample size
sink(file = here::here("data/output/text/samplesizeprivateabsenceihds.tex"))
cat(format(lm_absence_cycle_private$nobs, big.mark = ","))
sink(file = NULL)

# Official duty model
lm_work_cycle_private <- feols(
	absent_work ~ years_to_election + male + age +
	muslim + other_religion + upper_caste + sc_st + other_caste +
	distance_school,
	data = subset(df_ihds_clean, government_school == 0 & absent_school == 1),
	cluster = "state_district"
)

# Exporting election year mean
sink(file = here::here("data/output/text/electionmeanprivatedutyihds.tex"))
cat(
	round(
		mean(df_ihds_clean$absent_work[df_ihds_clean$government_school == 0 &
			df_ihds_clean$years_to_election == 0], na.rm = TRUE), 2
	)
)
sink(file = NULL)

election_mean_duty <- round(
		mean(df_ihds_clean$absent_work[df_ihds_clean$government_school == 0 &
			df_ihds_clean$years_to_election == 0], na.rm = TRUE), 2
	)

# Exporting sample size
sink(file = here::here("data/output/text/samplesizeprivatedutyihds.tex"))
cat(format(lm_work_cycle_private$nobs, big.mark = ","))
sink(file = NULL)

# Formatting table for export
models_list <- list(lm_absence_cycle_private, lm_work_cycle_private)

# Formatting footer
format_n <- function(x) format(round(x, 0), big.mark = ",")
gof <- list(list("raw" = "nobs", "clean" = "N", "fmt" = format_n))

rows <- tribble(
	~term, ~absencecycle, ~workcycle,
	"Election Year Mean", as.character(election_mean_absence), as.character(election_mean_duty)
)

# Putting table together with modelsummary
table_ihds_absence_cycle_private <- modelsummary(
	models_list,
	title = "Absence Over Electoral Cycle in Private Schools Using IHDS Data\\label{table:ihdsabsencecycleprivate}",
	coef_map = c(
		"years_to_election-2" = "2 or More Years Before Election",
		"years_to_election-1" = "1 Year Before Election",
		"years_to_election1" = "1 Year After Election",
		"years_to_election2" = "2 or More Years After Election",
		"male" = "Male",
		"age" = "Age",
		"muslim" = "Muslim",
		"other_religion" = "Other Religion",
		"upper_caste" = "Upper Caste",
		"sc_st" = "SC/ST",
		"other_caste" = "Other Caste",
		"distance_school" = "Distance from School"
	),
	stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    gof_map = gof,
    add_rows = rows,
    notes = "\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Robust standard errors in parentheses. Linear models of the probability of a teacher being absent from a private school the day IHDS surveyors surveyed the school, as well as the probability a teacher was absent on official duty as outlined in Equation \\ref{equation:businesscycle}.  The dependent variable is either a dummy variable that takes the value of one if the teacher was absent from the school on the day of the survey or, conditional on being absent, whether they were absent on official duty on the day of the survey.",
    escape = FALSE,
    output = "tinytable"
	) %>%
	group_tt(j = list("Absent" = 2, "On Official Duty" = 3)) %>%
	style_tt(i = -1:nrow(.), fontsize = 0.7)

table_ihds_absence_cycle_private %>%
	save_tt(
		paste0(path_tables, "/ihdsabsencecycleprivate.tex"),
		overwrite = TRUE
	)

# Plotting electoral cycle models #############################################

plot_absence <- tidyCyclePlots(
	lm_absence_cycle,
	interaction = FALSE,
	log_outcome = FALSE
)

plot_absence <- plot_absence + labs(x = "P(Absent)")

plot_absence_private <- tidyCyclePlots(
	lm_absence_cycle_private,
	interaction = FALSE,
	log_outcome = FALSE
)

plot_absence_private <- plot_absence_private + labs(x = "P(Absent)")

plot_work <- tidyCyclePlots(
	lm_work_cycle,
	interaction = FALSE,
	log_outcome = FALSE
)

plot_work <- plot_work + labs(x = "P(On Official Duty)")

plot_work_private <- tidyCyclePlots(
	lm_work_cycle_private,
	interaction = FALSE,
	log_outcome = FALSE
) 

plot_work_private <- plot_work_private + labs(x = "P(On Official Duty)")

plots_government <- list(plot_absence, plot_work)

plot_government <- wrap_plots(plots_government, ncol = 1, axes = "collect") +
	plot_layout(axes = "collect_x") +
	plot_annotation(tag_levels = "A")

ggsave(
	file = here(paste0(path_figures, "/absencecycleihds.pdf")),
    height = 0.5 * page_height,
    width = page_width,
    units = "in",
)

plots_private <- list(plot_absence_private, plot_work_private)

plot_private <- wrap_plots(plots_private, ncol = 1, axes = "collect") +
	plot_layout(axes = "collect_x") +
	plot_annotation(tag_levels = "A")

ggsave(
	file = here(paste0(path_figures, "/absencecycleprivateihds.pdf")),
    height = 0.5 * page_height,
    width = page_width,
    units = "in"
)

# Electoral cycle with share of elections model ###############################
# Asked for by a reviewer to look at the share of elections in a district

formula_absent <- as.formula(
	absent_school ~ years_to_election_all * share_constituencies_election +
	male + age + muslim + other_religion + upper_caste + sc_st + other_caste +
	distance_school
	)

formula_work <- as.formula(
	absent_work ~ years_to_election_all * share_constituencies_election + male +
	age + muslim + other_religion + upper_caste + sc_st + other_caste +
	distance_school
	)

lm_absence_cycle_interaction <- feols(
	formula_absent,
	data = subset(df_ihds_clean, government_school == 1),
	cluster = "state_district"
)

lm_absence_cycle_private_interaction <- feols(
	formula_absent,
	data = subset(df_ihds_clean, government_school == 0),
	cluster = "state_district"
)

lm_work_cycle_interaction <- feols(
	formula_work,
	data = subset(df_ihds_clean, government_school == 1 & absent_school == 1),
	cluster = "state_district"
)

lm_work_cycle_private_interaction <- feols(
	formula_work,
	data = subset(df_ihds_clean, government_school == 0 & absent_school == 1),
	cluster = "state_district"
)

models_list <- list(
	lm_absence_cycle_interaction, lm_absence_cycle_private_interaction,
	lm_work_cycle_interaction, lm_work_cycle_private_interaction
)

modelsummary(models_list, stars = TRUE)

# Formatting footer
format_n <- function(x) format(round(x, 0), big.mark = ",")
gof <- list(list("raw" = "nobs", "clean" = "N", "fmt" = format_n))

election_mean_absence_interaction <- round(
	lm_absence_cycle_interaction$coefficients[[1]], 3
)
election_mean_absence_private_interaction <- round(
	lm_absence_cycle_private_interaction$coefficients[[1]], 3
)
election_mean_duty_interaction <- round(
	lm_work_cycle_interaction$coefficients[[1]], 3
)
election_mean_duty_private_interaction  <- round(
	lm_work_cycle_private_interaction$coefficients[[1]], 3
)

rows <- tribble(
	~term, ~absencecycle, ~absencecycleprivate, ~workcycle, ~workcycleprivate,
	"Election Year Mean", as.character(election_mean_absence_interaction),
		as.character(election_mean_absence_private_interaction),
		as.character(election_mean_duty_interaction),
		as.character(election_mean_duty_private_interaction),
	"Type of School", "Government", "Private", "Government", "Private"
)

table_note <- "\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Robust standard errors in parentheses. Linear models of the probability of a teacher being absent from a government school the day IHDS surveyors surveyed the school, as well as the probability a teacher was absent on official duty as outlined in Equation \\ref{equation:businesscycle}. The dependent variable is either a dummy variable that takes the value of one if the teacher was absent from the school on the day of the survey or, conditional on being absent, whether they were absent on official duty on the day of the survey."

# Putting table together with modelsummary
table_ihds_absence_cycle_interaction <- modelsummary(
	models_list,
	title = "Absence Over Electoral Cycle in Government Schools Using IHDS Data\\label{table:ihdsabsencecycleinteraction}",
	coef_map = c(
		"years_to_election-2:share_constituencies_election" = "2 or More Years Before Election x Share of Constituencies Holding Elections",
		"years_to_election_all-1:share_constituencies_election" = "1 Year Before Election x Share of Constituencies Holding Elections",
		"years_to_election_all0:share_constituencies_election" = "Election Year x Share of Constituencies Holding Elections",
		"years_to_election_all1:share_constituencies_election" = "1 Year After Election x Share of Constituencies Holding Elections",
		"years_to_election_all2:share_constituencies_election" = "2 or More Years After Election x Share of Constituencies Holding Elections",
		"years_to_election_all-2" = "2 or More Years Before Election",
		"years_to_election_all-1" = "1 Year Before Election",
		"years_to_election_all1" = "1 Year After Election",
		"years_to_election_all2" = "2 or More Years After Election",
		"share_constituencies_election" = "Share of Constituencies Holding Elections"
	),
	stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    gof_map = gof,
    add_rows = rows,
    escape = FALSE,
    notes = table_note,
    output = "tinytable"
) %>%
	group_tt(j = list("Absent" = 2:3, "On Official Duty" = 4:5)) %>%
	style_tt(i = -2:nrow(.), fontsize = 0.7)

table_ihds_absence_cycle_interaction %>%
	save_tt(
		here(paste0(path_tables, "/ihdsabsencecycleinteraction.tex")),
		overwrite = TRUE
	)
