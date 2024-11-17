# Metadata ####################################################################
# Name: 003_dise_clean.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Cleans the DISE school report cards data and merges with electoral
# data for analysis
# Date created: 2020/12/22

# Loading packages ############################################################

library(arrow)
library(readstata13)
library(data.table)
library(zoo)
library(xtable)
library(stargazer)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(tidylog)
library(future.apply)
plan(multisession)
library(rio)
library(here)

# Loading data and sourcing functions #########################################

df_src <- read_parquet(here("data/raw/dise/src.parquet"),
    col_select = c(school_code, state_district, year, sch_management,
        rural, teachers_non_teaching_days, teachers_non_teaching_numbers,
        teachers_male, teachers_female, teachers_nr, teachers_head,
        smc_meetings, visits_crc, visits_brc))

# Loading location data:
df_src_locations <- read_parquet(here("data/raw/dise/src_locations.parquet"),
    col_select = c(school_code, constituency_code_post, constituency_code_pre,
        state_code, merge_village, merge_constituency))

# Loading election year data:
df_elections <- import(here("data/clean/coalition_parties.rds"))

# Sourcing functions:
source(here("scripts/functions.R"))

# Cleaning school report cards data ###########################################

setDT(df_src)
varcols <- c("school_code", "sch_management")
df_src_full <- df_src[rowSums(is.na(df_src[, ..varcols])) == 0, ]
df_src_full <- df_src_full[df_src_full$sch_management != 0, ]

df_src_full <- df_src_full %>%
	mutate(
	    school_code = as.character(school_code),
        school_code = ifelse(nchar(school_code) == 10, paste0("0", school_code), school_code),
		private = ifelse(sch_management > 96 | sch_management == 4 | sch_management == 5 | sch_management == 8, 1, 0),
		teachers = teachers_male + teachers_female + teachers_nr + teachers_head,
		teachers = ifelse(teachers == 0, NA, teachers),
		average_absence = teachers_non_teaching_days / teachers,
        ihs_average_absence = ifelse(teachers_non_teaching_days == 0,
            log(teachers_non_teaching_days + 0.1),
            log(teachers_non_teaching_days)),
		absent = ifelse(teachers_non_teaching_days > 0, 1, 0),
		absent = ifelse(is.na(teachers_non_teaching_days), NA, absent),
        administrative_visits = visits_brc + visits_crc,
        year = year - 1,
        original = 1
	) %>%
    select(
        c(school_code, state_district, year, rural, private,
            teachers, average_absence, ihs_average_absence, absent,
            administrative_visits, smc_meetings, original,
            teachers_non_teaching_days)
    )

df_src <- df_src_full %>%
    select(c(school_code, year, original)) %>%
    as_tibble()

# Fill panel:
df_panel <- df_src %>%
    select(school_code) %>%
    unique() %>%
    setDT()

panel_years <- c(2005:2017)
df_panel <- df_panel[rep(1:.N, length(panel_years))][, year := panel_years, by = c("school_code")]
df_panel <- df_panel %>%
    as_tibble()

# Merge with SRC data to complete panel with observations:
df_src <- full_join(df_src, df_panel, by = c("school_code", "year"))

# Saving on memory
rm(df_panel)

# Cleaning SRC Locations ######################################################

# Adding merge variable to school locations and removing duplicates:
df_src_locations <- df_src_locations %>%
    unique()

# Merging school report card data with school locations #######################

df_src <- left_join(df_src, df_src_locations, by = "school_code")

df_src <- as_tibble(df_src)

# Merging electoral data with src data ########################################

# Creating dummy years to extend matching of elections before and after DISE
# data years

setDT(df_src)

df_school_code <- df_src %>%
    select(
        school_code, state_code, constituency_code_pre, constituency_code_post
    ) %>%
    distinct(school_code, .keep_all = TRUE)

out_of_sample_years <- c(1999:2004, 2018:2020)
df_school_code <- df_school_code[rep(1:.N, length(out_of_sample_years))][, year := out_of_sample_years, by = c("school_code")]

# Carrying electoral constituencies forward and back
setDT(df_school_code)

# Sorting data:
setorder(df_school_code, school_code, year)

# Carrying forward and back constituency and state codes
df_school_code[, constituency_code_pre := na.locf(constituency_code_pre, na.rm = F),
    by = c("school_code")]
df_school_code[, constituency_code_post := na.locf(constituency_code_post, na.rm = F, fromLast = TRUE),
    by = c("school_code")]
df_school_code[, state_code := na.locf(state_code, na.rm = F),
    by = c("school_code")]
df_school_code[, state_code := na.locf(state_code, na.rm = F, fromLast = TRUE),
    by = c("school_code")]

df_src <- full_join(df_src, df_school_code, by = c("school_code", "year")) %>%
    mutate(
        state_code.x = ifelse(year < 2005 | year > 2017, state_code.y, state_code.x),
        constituency_code_pre.x = ifelse(year < 2005, constituency_code_pre.y, constituency_code_pre.x),
        constituency_code_post.x = ifelse(year > 2017, constituency_code_post.y, constituency_code_post.x)
    ) %>%
    select(
        -c(state_code.y, constituency_code_pre.y, constituency_code_post.y)
    ) %>%
    rename(
        state_code = state_code.x,
        constituency_code_pre = constituency_code_pre.x,
        constituency_code_post = constituency_code_post.x
    ) %>%
    as_tibble()

df_src <- setDT(df_src)

# Removing df_school_code for memory
rm(df_school_code)
df_src <- as_tibble(df_src)

# Splitting DISE data by whether it was pre- or post-delimitation to merge with
# electoral data

df_src_pre_delimitation <- df_src %>%
    filter(year <= 2007) %>%
    rename(st_code_ec = state_code)

# Merge pre-delimitation with electoral data:
df_src_pre_delimitation <- left_join(df_src_pre_delimitation,
    df_elections, by = c("st_code_ec", "constituency_code_pre", "year"))

df_elections <- setDT(df_elections)

# Updating merge report with pre-delimitation electoral constituency merge:
df_src_pre_delimitation <- setDT(df_src_pre_delimitation)

df_src_pre_delimitation <- df_src_pre_delimitation %>%
    rename(constituency_code_post = constituency_code_post.x) %>%
    mutate(delimitation = "Pre-Delimitation") %>%
    select(-c(constituency_code_post.y))

# Matching post-delimitation data:
df_src_post_delimitation <- df_src %>%
    filter(year > 2007) %>%
    rename(st_code_ec = state_code)

# Merge post-delimitation with electoral data:
df_src_post_delimitation <- left_join(df_src_post_delimitation,
    df_elections, by = c("st_code_ec", "constituency_code_post", "year")) %>%
    as_tibble()
    
df_elections <- setDT(df_elections)

# Updating merge report with post-delimitation electoral constituency merge:
df_src_post_delimitation <- setDT(df_src_post_delimitation)

df_src_post_delimitation <- df_src_post_delimitation %>%
    rename(constituency_code_pre = constituency_code_pre.x) %>%
    mutate (delimitation = "Post-Delimitation") %>%
    select(-c(constituency_code_pre.y))

# Appending school report cards data:
df_src <- rbind(df_src_post_delimitation, df_src_pre_delimitation)

# Removing data frames for memory:
rm(df_src_post_delimitation, df_src_pre_delimitation)

###############################################################################
# Generating election variables
###############################################################################

df_src <- df_src %>%
    mutate(
        next_election_year = ifelse(election_year == 1, year, NA),
        previous_election_year = ifelse(election_year == 1, year, NA),
        election_year = ifelse(is.na(election_year), 0, election_year),
        next_enop = ifelse(election_year == 1, enop, NA),
        previous_enop = ifelse(election_year == 1, enop, NA),
        next_margin = ifelse(election_year == 1, margin, NA),
        previous_margin = ifelse(election_year == 1, margin, NA),
        next_turnout = ifelse(election_year == 1, turnout, NA),
        previous_turnout = ifelse(election_year == 1, turnout, NA),
        next_coalition = ifelse(election_year == 1, coalition, NA),
        previous_coalition = ifelse(election_year == 1, coalition, NA),
        by_election = ifelse(election_year == 1, by_election, NA),
        previous_by_election = ifelse(election_year == 1, by_election, NA),
        next_by_election = ifelse(election_year == 1, by_election, NA)
    )

# Creating previous and last election year variables
setDT(df_src)

# Sorting data:
setorder(df_src, school_code, year)

# Carrying forward and back election variables
df_src[, previous_election_year := na.locf(previous_election_year, na.rm = F),
    by = c("school_code")]
df_src[, previous_enop := na.locf(previous_enop, na.rm = F),
    by = c("school_code")]
df_src[, previous_margin := na.locf(previous_margin, na.rm = F),
    by = c("school_code")]
df_src[, previous_turnout := na.locf(previous_turnout, na.rm = F),
    by = c("school_code")]
df_src[, previous_coalition := na.locf(previous_coalition, na.rm = F),
    by = c("school_code")]
df_src[, previous_by_election := na.locf(previous_by_election, na.rm = F),
    by = c("school_code")]

df_src[, next_election_year := na.locf(next_election_year,
    na.rm = F,
    fromLast = TRUE),
    by = c("school_code")]
df_src[, next_enop := na.locf(next_enop, na.rm = F, fromLast = TRUE),
    by = c("school_code")]
df_src[, next_margin := na.locf(next_margin, na.rm = F, fromLast = TRUE),
    by = c("school_code")]
df_src[, next_turnout := na.locf(next_turnout, na.rm = F, fromLast = TRUE),
    by = c("school_code")]
df_src[, next_coalition := na.locf(next_coalition, na.rm = F, fromLast = TRUE),
    by = c("school_code")]
df_src[, next_by_election := na.locf(next_by_election, na.rm = F, fromLast = TRUE),
    by = c("school_code")]

# Distance to previous and next election:

df_src <- df_src %>%
    mutate(
        distance_to_previous_election = year - previous_election_year,
        distance_to_next_election = year - next_election_year,
        distance_to_closest_election = ifelse(abs(distance_to_previous_election)
            < abs(distance_to_next_election),
            distance_to_previous_election,
            distance_to_next_election
        ),
        distance_to_closest_election = ifelse(next_election_year %in%
            NA, distance_to_previous_election, distance_to_closest_election),
        distance_to_closest_election = ifelse(election_year == 1,
            0, distance_to_closest_election
        ),
        vs_year_0 = ifelse(distance_to_closest_election <= -2, 1, 0),
        vs_year_1 = ifelse(distance_to_closest_election == -1, 1, 0),
        vs_year_2 = ifelse(election_year == 1, 1, 0),
        vs_year_3 = ifelse(distance_to_closest_election == 1, 1, 0),
        vs_year_4 = ifelse(distance_to_closest_election >= 2, 1, 0),
        constituency_code = ifelse(year <= 2007, constituency_code_pre,
            constituency_code_post),
        years_to_election = distance_to_next_election
    ) %>%
    filter(
        year >= 2005 & year <= 2017,
        original == 1
    ) %>%
    mutate(
        analytic_margin = case_when(
            vs_year_0 == 1 | vs_year_1 == 1 ~ next_margin,
            vs_year_2 == 1 ~ margin,
            vs_year_3 == 1 | vs_year_4 == 1 ~ previous_margin,
        ),
        analytic_enop = case_when(
            vs_year_0 == 1 | vs_year_1 == 1 ~ next_enop,
            vs_year_2 == 1 ~ enop,
            vs_year_3 == 1 | vs_year_4 == 1 ~ previous_enop
        ),
        analytic_alignment = case_when(
            vs_year_2 != 1 ~ previous_coalition,
            vs_year_2 == 1 ~ coalition,
        )
    ) %>%
    as_tibble()

# Merging IDs with full school data:

df_src <- left_join(df_src_full, df_src, by = c("school_code", "year")) %>%
    select(-original.x, -original.y)

# Calculating lags
setDT(df_src)
setorder(df_src, school_code, year)

df_src[, lag_absent := shift(absent), by = school_code]
df_src[, lag_ihs_average_absence := shift(ihs_average_absence), by = school_code]
df_src[, lag_smc_meetings := shift(smc_meetings), by = school_code]
df_src[, lag_administrative_visits := shift(administrative_visits), by = school_code]
df_src[, lag_previous_coalition := shift(previous_coalition), by = school_code]
df_src[, previous_coalition := fifelse(
    election_year == 1,
    lag_previous_coalition,
    previous_coalition)
]
df_src[, lag_teachers := shift(teachers), by = school_code]

# Carrying state names forward and back:
setDT(df_src)
setorder(df_src, state_district, year)

df_src[, state_name := na.locf(state_name, na.rm = F),
    by = c("state_district")]
df_src[, state_name := na.locf(state_name, na.rm = F, fromLast = TRUE),
    by = c("state_district")]

# Creating upper and lower treatment effect bounds ############################

df_src[, max_absent := max(absent), by = school_code]
df_src[, max_ihs_absent := max(ihs_average_absence), by = school_code]
df_src[, min_absent := min(absent), by = school_code]
df_src[, min_ihs_absent := min(ihs_average_absence), by = school_code]

# Final cleaning of data ######################################################

# Producing matching rate data set
df_src_matching <- df_src %>%
    select(
        teachers, average_absence, year, absent, administrative_visits,
        smc_meetings, school_code, state_name, rural, private
    ) %>%
    left_join(., df_src_locations, df_src, by = "school_code")

df_src <- df_src[!is.na(st_code_ec), ]
df_src[, unique_constituency_code := paste0(st_code_ec, constituency_code)]

# Saving data #################################################################

write_parquet(df_src_matching, sink = "data/clean/src_matching.parquet")
saveRDS(df_src, file = "data/clean/absence_src_clean.rds")

