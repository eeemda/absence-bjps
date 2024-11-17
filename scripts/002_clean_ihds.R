# Metadata ####################################################################
# Name: 001_clean_ihds.R
# Purpose: Cleans IHDS School and teacher level data from 2011
# Date created: 2020/09/16

# Load Packages ###############################################################

library(dplyr)
library(stringr)
library(lubridate)
library(rio)
library(here)

# Load data ###################################################################

# Loading School Staff data
load(file = here::here("data/raw/ihds/36151-0008-Data.rda"))
df_ihds_staff <- da36151.0008
rm(da36151.0008)

# Loading School data:
load(file = here::here("data/raw/ihds/36151-0009-Data.rda"))
df_ihds_school <- da36151.0009
rm(da36151.0009)

# Loading individual level data:
load(file = here::here("data/raw/ihds/36151-0001-Data.rda"))
df_ihds_individual <- da36151.0001
rm(da36151.0001)

# Source functions
source(file.path(here(), "scripts/functions.R"))

# Cleaning school data ########################################################

# Cleaning school level data:
df_ihds_school_clean <- clean_identifiers(df_ihds_school) %>%
    clean_ihds_survey_dates(.) %>%
    mutate(
        teachers = PS24A + PS24B,
        private = ifelse(PS1 == "(1) Government 1", 0, 1)
    ) %>%
    rename(students = PS6) %>%
    mutate(
        state = as.numeric(state),
        state_district = case_when(
            nchar(as.character(state)) == 1 ~ str_sub(as.character(id_psu), start = 1, end = 3),
            TRUE ~ str_sub(as.character(id_psu), start = 1, end = 4)
        )
    ) %>%
    select(
        state_name, state, district_code, state_district, village, id_psu,
        school_id, day_survey, month_survey, year_survey, date_survey, teachers,
        private, students
    ) %>%
    filter(!is.na(date_survey))

df_ihds_school_clean_missing_district <- df_ihds_school_clean %>%
    filter(district_code == 0)
View(df_ihds_school_clean_missing_district)
    
# Selecting and cleaning variables in staff data ##############################

df_ihds_staff_clean <- clean_identifiers(df_ihds_staff) %>%
    mutate(
        government_school = ifelse(SQGOVT == "(1) Govt", 1, 0),
        private_school = ifelse(SQGOVT == "(2) Pvt", 1, 0),
        absent_interview = ifelse(SS4 == "(3) Neither 3", 1, 0),
        absent_school = ifelse(SS6 == "(1) Yes 1", 0, 1),
        absent_work = ifelse(SS6 == "(3) Away-official work 3", 1, 0),
        male = ifelse(SS7 == "(1) Male 1", 1, 0),
        state_name = case_when(
            STATEID == 1 ~ "Jammu & Kashimr",
            STATEID == 2 ~ "Himachal Pradesh",
            STATEID == 3 ~ "Punjab",
            STATEID == 4 ~ "Chandigarh",
            STATEID == 5 ~ "Uttarakhand",
            STATEID == 6 ~ "Haryana",
            STATEID == 7 ~ "Delhi",
            STATEID == 8 ~ "Rajasthan",
            STATEID == 9 ~ "Uttar Pradesh",
            STATEID == 10 ~ "Bihar",
            STATEID == 11 ~ "Sikkim",
            STATEID == 12 ~ "Arunachal Pradesh",
            STATEID == 13 ~ "Nagaland",
            STATEID == 14 ~ "Manipur",
            STATEID == 15 ~ "Mizoram",
            STATEID == 16 ~ "Tripura",
            STATEID == 17 ~ "Meghalaya",
            STATEID == 18 ~ "Assam",
            STATEID == 19 ~ "West Bengal",
            STATEID == 20 ~ "Jharkhand",
            STATEID == 21 ~ "Orissa",
            STATEID == 22 ~ "Chhattisgarh",
            STATEID == 23 ~ "Madhya Pradesh",
            STATEID == 24 ~ "Gujarat",
            STATEID == 25 ~ "Daman & Diu",
            STATEID == 26 ~ "Dadra and Nagar Haveli",
            STATEID == 27 ~ "Maharashtra",
            STATEID == 28 ~ "Andhra Pradesh",
            STATEID == 29 ~ "Karnataka",
            STATEID == 30 ~ "Goa",
            STATEID == 31 ~ "Lakshadweep",
            STATEID == 32 ~ "Kerala",
            STATEID == 33 ~ "Tamil Nadu",
            STATEID == 34 ~ "Pondicherry",
            STATEID == 35 ~ "Andaman & Nicobar Islands",
            TRUE ~ NA_character_
        ),
        district_code = as.numeric(district)
    ) %>%
    rename(
        teacher_class = SS3,
        age = SS8,
        religion = SS11,
        caste = SS12,
        distance_school = SS13
    ) %>%
    select(
        c(
            state_name, district_code, village, id_psu, school_id,
            government_school, private_school, absent_interview, absent_school,
            absent_work, male, age, religion, caste, distance_school,
            teacher_class
        )
    ) %>%
    mutate(
        age = ifelse(age < 18, NA, age),
        hindu = ifelse(religion == "(1) Hindu 1", 1, 0),
        muslim = ifelse(religion == "(2) Muslim 2", 1, 0),
        other_religion = ifelse(religion != "(1) Hindu 1" & religion != "(2) Muslim 2", 1, 0),
        upper_caste = ifelse(caste == "(1) Brahmin 1" | caste == "(2) Forward caste 2", 1, 0),
        obc = ifelse(caste == "(3) OBC 3", 1, 0),
        sc_st = ifelse(caste == "(4) SC 4" | caste == "(5) ST 5", 1, 0),
        other_caste = ifelse(caste == "(6) Others 6", 1, 0)
    )

# Cleaning student level data #################################################

df_ihds_student_clean <- df_ihds_individual %>%
    mutate(SCHOOLID = ifelse(is.na(CH22), 8, CH22)) %>%
    clean_identifiers(.) %>%
    filter(TA3 == "(1) Yes, currently 1") %>%
    mutate(
        male = ifelse(RO3 == "(1) Male 1", 1, 0),
        TA6 = as.numeric(TA6),
        teacher_treatment = case_when(
            TA6 == 1 ~ 1,
            TA6 == 2 ~ (1 / 2),
            TA6 == 3 ~ 0
        ),
        CH6 = as.numeric(CH6),
        teacher_absent = case_when(
            CH6 == 3 ~ 1,
            CH6 == 2 ~ (1 / 2),
            CH6 == 1 ~ 0
        ),
        CH8 = as.numeric(CH8),
        teacher_local = ifelse(CH8 == 2, 1, 0),
        CS4 = as.numeric(CS4),
        private = case_when(
            CS4 < 3 ~ 0,
            CS4 >= 3 & CS4 < 8 ~ 1
        ),
        score_reading = (as.numeric(TA8B) - 1) / 4,
        score_math = (as.numeric(TA9B) - 1) / 3,
        score_writing = (as.numeric(TA10B) - 1) / 2,
        score_total = (score_reading + score_math + score_writing) / 3,
        district = as.numeric(district)
    ) %>%
    rename(
        age = RO5,
        class = TA4,
        school_code = SCHOOLID
    ) %>%
    select(
        id_psu, school_id, state, district_code, village, school_code,
        everything()
    ) %>%
    select(
        district_code, id_psu, school_id, score_reading, score_math,
        score_writing, score_total, private, teacher_treatment, teacher_absent,
        teacher_local, male, age, class
    )

# Saving clean data ###########################################################

save(df_ihds_school_clean, file = here::here("data/clean/ihds_schools.RData"))
save(df_ihds_staff_clean, file = here::here("data/clean/ihds_staff.RData"))
save(df_ihds_student_clean, file = here::here("data/clean/ihds_student.RData"))
