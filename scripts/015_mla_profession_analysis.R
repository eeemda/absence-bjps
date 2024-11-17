# Metadata ####################################################################
# Name: 017_mla_profession_analysis.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Provides summary statistics on the number of MLAs that work in
# education and as teachers
# Date created: 2023/10/16

# Load packages ###############################################################

library(dplyr)
library(stringr)
library(janitor)
library(rio)
library(here)

# Cleaning electoral data #####################################################

# Function to bind electoral data
bindElections <- function(file) {
    print(file)
    election_df <- import(file, fill = TRUE) %>%
        select(
            State_Name, TCPD_Prof_Main, TCPD_Prof_Main_Desc, TCPD_Prof_Second,
            TCPD_Prof_Second_Desc, Position
        ) %>%
        clean_names() %>%
        mutate(
            education = ifelse(tcpd_prof_main == "Education" | tcpd_prof_second == "Education", 1, 0),
            teacher = ifelse(tcpd_prof_main_desc == "School Teacher" | tcpd_prof_second_desc == "School Teacher", 1, 0),
            mla = ifelse(tcpd_prof_main == "MLA" | tcpd_prof_second == "MLA", 1, 0)
        ) %>%
        filter(tcpd_prof_main != "") %>%
        select(state_name, education, teacher, position)

    return(election_df)
}

# Listing all files:
affidavit_files <- list.files(path = here("data/raw/election affidavit/"),
    full.names = TRUE)

affidavit_df <- lapply(affidavit_files, bindElections) %>%
    bind_rows()

summary(affidavit_df)
table(affidavit_df$education)
table(affidavit_df$position, affidavit_df$education)
table(affidavit_df$teacher)
table(affidavit_df$position, affidavit_df$teacher)