###############################################################################
# Name: 004_summary_statistics_tables_server.R
# Purpose: Create summary statitistics tables for Absence paper
# Author: Emmerich Davies
# Date created: 2020/10/30
###############################################################################

###############################################################################
# Load Packages
###############################################################################

library(dplyr)
library(tidyr)
library(broom)
library(xtable)
library(readstata13)
library(here)

###############################################################################
# Loading data
###############################################################################

# Loading DISE data
df_src <- readRDS(here::here("data/clean/absence_src_clean.rds"))

# Loading IHDS data
load(file = here::here("data/clean/ihds_schools_elections.RData"))
load(file = here::here("data/clean/ihds_staff.RData"))
load(file = here::here("data/clean/ihds_student.RData"))

# Loading Functions:
source(here::here("scripts/functions.R"))

###############################################################################
# Summary statistics of DISE data
###############################################################################

vars <- c("absent", "rural", "teachers")
df_src$rural <- ifelse(df_src$rural == "Rural", 1, 0)
df_dise_statistics <- lapply(vars, SummaryStatisticsTable)
df_dise_statistics <- bind_rows(df_dise_statistics)

df_dise_summary <- df_dise_statistics %>%
	mutate(
		variable = c("Absent (%)", "Rural (%)", "Number of Teachers"),
		difference = signif(summary_statistics7, 3),
		difference = ifelse(abs(summary_statistics7) >= (summary_statistics8 * 2.58), paste0(difference, "***"), difference)
	) %>%
	select(
		variable, summary_statistics1:summary_statistics6, difference, summary_statistics9:summary_statistics11
	)

summary_stats <- xtable(df_dise_summary,
	align = "llrrrrrrrrrr",
	digits = c(0, 0, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0))
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c(" & \\multicolumn{3}{c}{Government Schools} & 
	\\multicolumn{3}{c}{Private Schools} & Difference & \\multicolumn{3}{c}{All Schools} \\\\\n",
	"\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){9-11} \n",
	" & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} \\\\\n")

print(summary_stats, file = here::here("data/output/tables/summarydise.tex"),
	include.colnames = F, include.rownames = F, booktabs = T, floating = F,
	add.to.row = addtorow, format.args = list(big.mark = ","),
	only.contents = T)

###############################################################################
# Summary Statistics of IHDS Data
###############################################################################

# School Level Summary Stats by Private and Government
df_school <- df_ihds_school_clean %>%
	select(students, teachers, private) %>%
	filter(private == 0 | private == 1) %>%
	group_by(private) %>%
	summarise_all(
		list(~ mean(., na.rm = T), ~sd(., na.rm = T), ~sum(!is.na(.)))
	) %>%
	pivot_longer(
		cols = 2:7,
		names_to = "variable"
	) %>%
	mutate(
		stat = c(rep("mean", 2), rep("sd", 2), rep("n", 2),
			rep("mean", 2), rep("sd", 2), rep("n", 2)),
		variable = sub("_mean", "", variable),
		variable = sub("_sd", "", variable),
		variable = sub("_sum", "", variable)
	) %>%
	pivot_wider(
		names_from = c("private", "stat"),
		values_from = "value"
	) %>%
	mutate(variable = c("Number of Students", "Number of Teachers"))

# Getting differences in values:
dvs <- c("students", "teachers")

df_school_difference <- lapply(dvs, runLM, indvar = "private", dataframe = df_ihds_school_clean) %>%
	bind_rows() %>%
	filter(term == "private") %>%
	mutate(
		difference = signif(estimate, 3),
		difference = ifelse(statistic >= 2.58, paste0(difference, "***"), difference),
		difference = ifelse(statistic >= 1.96 & statistic < 2.58, paste0(difference, "**"), difference),
		difference = ifelse(statistic >= 1.645 & statistic < 1.96, paste0(difference, "*"), difference)		
	) %>%
	select(difference)

# School level summary stats overall:
df_school_overall <- df_ihds_school_clean %>%
	select(students, teachers) %>%
	summarise_all(
		list(~ mean(., na.rm = T), ~ sd(., na.rm = T), ~sum(!is.na(.)))
	) %>%
	pivot_longer(
		cols = 1:6,
		names_to = "variable"
	) %>%
	mutate(
		stat = c(rep("mean", 2), rep("sd", 2), rep("n", 2)),
		variable = sub("_mean", "", variable),
		variable = sub("_sd", "", variable),
		variable = sub("_sum", "", variable)
	) %>%
	pivot_wider(
		names_from = c("stat"),
		values_from = "value"
	) %>%
	select(c(mean, sd, n))

# Exporting IHDS School-Level Summary Statistics
df_school <- cbind(df_school, df_school_difference, df_school_overall)

summary_stats <- xtable(df_school,
	align = "llrrrrrrrrrr",
	digits = c(0, 0, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0))
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c(" & \\multicolumn{3}{c}{Government Schools} & 
	\\multicolumn{3}{c}{Private Schools} & Difference & \\multicolumn{3}{c}{All Schools} \\\\\n",
	"\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){9-11} \n",
	" & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} \\\\\n")

print(summary_stats, file = here::here("data/output/tables/summaryihdsschool.tex"),
	include.colnames = F, include.rownames = F, booktabs = T, floating = F,
	add.to.row = addtorow, format.args = list(big.mark = ","),
	only.contents = T)

###############################################################################
# Teacher Level Summary Statistics
###############################################################################

# Summary statistics for teachers in Private and Government Schools:
df_summary <- df_ihds_staff_clean %>%
	select(
		private_school, absent_school, absent_work, male, age, hindu, muslim,
		other_religion, upper_caste, obc, sc_st, other_caste, distance_school
	)

# Sumamry statistics for private and government schools
df_summary_group <- df_summary %>%
	group_by(
		private_school
	) %>%
	summarise_all(
		list(~ mean(., na.rm = T), ~ sd(., na.rm = T), ~sum(!is.na(.)))
	) %>%
	pivot_longer(
		cols = 2:37,
		names_to = "variable"
	) %>%
	mutate(
		stat = c(rep("mean", 12), rep("sd", 12), rep("n", 12),
			rep("mean", 12), rep("sd", 12), rep("n", 12)),
		variable = sub("_mean", "", variable),
		variable = sub("_sd", "", variable),
		variable = sub("_sum", "", variable)
	) %>%
	pivot_wider (
		names_from = c("private_school", "stat"),
		values_from = "value"
	) %>%
	mutate(
		variable = c("Absent from school (%)", "Absent on official duty (%)",
			"Male (%)", "Age", "Hindu (%)", "Muslim (%)",
			"Other Religion (%)", "Upper Caste (%)", "OBC (%)",	"SC/ST (%)",
			"Other Caste (%)", "Distance from school (km)")
	)

# Differences in values between teachers in private and government schools

# Defining variables:
dvs <- c("absent_school", "absent_work", "male", "age", "hindu", "muslim",
	"other_religion", "upper_caste", "obc", "sc_st", "other_caste",
	"distance_school")

df_teacher_difference <- lapply(dvs, runLM, indvar = "private_school", dataframe = df_summary) %>%
	bind_rows() %>%
	filter(
		term == "private_school"
	) %>%
	mutate(
		difference = round(estimate, 3),
		difference = ifelse(statistic >= 2.58, paste0(difference, "***"), difference),
		difference = ifelse(statistic >= 1.96 & statistic < 2.58, paste0(difference, "**"), difference),
		difference = ifelse(statistic >= 1.645 & statistic < 1.96, paste0(difference, "*"), difference)		
	) %>%
	select(
		difference
	)

# Summary statistics for overall sample
df_summary_overall <- df_summary %>%
	select(
		-private_school
	) %>%
	summarise_all(
		list(~ mean(., na.rm = T), ~ sd(., na.rm = T), ~sum(!is.na(.)))
	) %>%
	pivot_longer(
		cols = 1:36,
		names_to = "variable"
	) %>%
	mutate(
		stat = c(rep("mean", 12), rep("sd", 12), rep("n", 12)),
		variable = sub("_mean", "", variable),
		variable = sub("_sd", "", variable),
		variable = sub("_sum", "", variable)
	) %>%
	pivot_wider(
		names_from = c("stat"),
		values_from = "value"
	) %>%
	select(
		c(mean, sd, n)
	)

summary_stats <- cbind(df_summary_group, df_teacher_difference, df_summary_overall)

summary_stats <- xtable(summary_stats,
	align = "llrrrrrrrrrr",
	digits = c(0, 0, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0))
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c(" & \\multicolumn{3}{c}{Government Schools} & 
	\\multicolumn{3}{c}{Private Schools} & Difference & \\multicolumn{3}{c}{All Schools} \\\\\n",
	"\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){9-11} \n",
	" & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} \\\\\n")

print(summary_stats, file = here::here("data/output/tables/summaryihdsteacher.tex"),
	include.colnames = F, include.rownames = F, booktabs = T, floating = F,
	add.to.row = addtorow, format.args = list(big.mark = ","),
	only.contents = T)

###############################################################################
# Student Level Summary Statistics
###############################################################################

df_ihds_student <- df_ihds_student_clean %>%
    select(-c(teacher_treatment, teacher_absent, school_id, id_psu))

df_student <- df_ihds_student %>%
    select(
        starts_with("score"), private, teacher_local, male, age, class
    ) %>%
    filter(
        !is.na(private)
    ) %>%
    group_by(
        private
    ) %>%
    summarise_all(
        list(~ mean(., na.rm = T), ~sd(., na.rm = T), ~sum(!is.na(.)))
    ) %>%
    pivot_longer(
        cols = 2:25,
        names_to = "variable"
    ) %>%
    mutate(
        stat = c(rep("mean", 8), rep("sd", 8), rep("n", 8),
            rep("mean", 8), rep("sd", 8), rep("n", 8)),
        variable = sub("_mean", "", variable),
        variable = sub("_sd", "", variable),
        variable = sub("_sum", "", variable)
    ) %>%
    pivot_wider(
        names_from = c("private", "stat"),
        values_from = "value"
    ) %>%
    mutate(
        variable = c("Reading", "Math", "Writing", "Overall Score",
            "Local Teacher", "Male", "Age", "Grade")
    )

# Differences between students in government and private schools
# Defining variables:
dvs <- c("score_reading", "score_math", "score_writing", "score_total",
    "teacher_local", "male", "age", "class")

df_student_difference <- lapply(dvs, runLM, indvar = "private",
    dataframe = df_ihds_student) %>%
    bind_rows() %>%
    filter(
        term == "private"
    ) %>%
    mutate(
        difference = round(estimate, 3),
        difference = ifelse(statistic >= 2.58, paste0(difference, "***"),
            difference),
        difference = ifelse(statistic >= 1.96 & statistic < 2.58,
            paste0(difference, "**"), difference),
        difference = ifelse(statistic >= 1.645 & statistic < 1.96,
            paste0(difference, "*"), difference)     
    ) %>%
    select(
        difference
    )

# Summary statistics for overall sample
df_summary_overall <- df_ihds_student %>%
    select(-c(private, district_code)) %>%
    summarise_all(
        list(~ mean(., na.rm = T), ~ sd(., na.rm = T), ~sum(!is.na(.)))
    ) %>%
    pivot_longer(
        cols = 1:24,
        names_to = "variable"
    ) %>%
    mutate(
        stat = c(rep("mean", 8), rep("sd", 8), rep("n", 8)),
        variable = sub("_mean", "", variable),
        variable = sub("_sd", "", variable),
        variable = sub("_sum", "", variable)
    ) %>%
    pivot_wider(
        names_from = c("stat"),
        values_from = "value"
    ) %>%
    select(c(mean, sd, n))

summary_stats <- cbind(df_student, df_student_difference,
    df_summary_overall)

summary_stats <- xtable(summary_stats,
    align = "llrrrrrrrrrr",
    digits = c(0, 0, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0))
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c(" & \\multicolumn{3}{c}{Government Schools} & 
    \\multicolumn{3}{c}{Private Schools} & Difference & \\multicolumn{3}{c}{All Schools} \\\\\n",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){9-11} \n",
    " & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} & & \\multicolumn{1}{r}{Mean} & \\multicolumn{1}{r}{SD} & \\multicolumn{1}{r}{N} \\\\\n")

print(summary_stats,
    file = here::here("data/output/tables/summaryihdsstudent.tex"),
    include.colnames = F, include.rownames = F, booktabs = T, floating = F,
    add.to.row = addtorow, format.args = list(big.mark = ","),
    only.contents = T)