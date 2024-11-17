# Metadata ####################################################################
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Name: 016_chaudhury_comparison.R
# Purpose: Compare absenteeism rates between IHDS and Chaudhury et al. 2005
# Date Created: 2023/08/10

# Loading packages ############################################################

library(arrow)
library(dplyr)
library(ggplot2)
library(NatParksPalettes)
library(srvyr)
library(patchwork)
library(xtable)
library(tidylog)
library(janitor)
library(rio)
library(here)

# Load data and functions #####################################################

load(file = here::here("data/clean/ihds_staff.RData"))
df_src <- read_parquet(here("data/raw/dise/src.parquet"),
    col_select = c(state_code_2001, year, sch_management,
        teachers_non_teaching_days, workdays, teachers_male, teachers_female)
)

# Loading individual level data to get weights
load(file = here::here("data/raw/ihds/36151-0001-Data.rda"))
df_ihds_individual <- da36151.0001
rm(da36151.0001)

# Declaring colour for plotting
single_colour <- natparks.pals(name = "Acadia", n = 1, type = "continuous")
two_colours <- natparks.pals(name = "Acadia", n = 2, type = "continuous")

# Getting IHDS Weights ########################################################

df_ihds_weights <- df_ihds_individual %>%
    select(IDPSU, WT, SURVEY) %>%
    clean_names() %>%
    unique() %>%
    rename(
        weights = wt,
        id_psu = idpsu
    ) %>%
    filter(!is.na(weights))

# Calculating absenteeism in IHDS by state ####################################

df_ihds_staff <- df_ihds_staff_clean %>%
    select(state_name, id_psu, absent_school, government_school) %>%
    left_join(., df_ihds_weights, by = "id_psu") %>%
    filter(!is.na(weights)) %>%
    as_survey_design(weights = weights) %>%
    select(-id_psu) %>%
    group_by(state_name, government_school) %>%
    summarise(
        absence_ihds = (survey_mean(absent_school, na.rm = TRUE) * 100)
    ) %>%
    filter(state_name != "Nagaland")

# Create state-level DISE data ################################################

df_src_clean <- df_src %>%
    mutate(teachers = teachers_male + teachers_female) %>%
    filter(teachers != 0 & !is.na(teachers)) %>%
    mutate(
        private = ifelse(sch_management > 96 | sch_management == 4 | sch_management == 5 | sch_management == 8, 1, 0),
        average_absence = ((teachers_non_teaching_days /
            (teachers_male + teachers_female)) / workdays) * 100
    ) %>%
    group_by(state_code_2001, private) %>%
    summarise(absence_dise = mean(average_absence, na.rm = TRUE)) %>%
    mutate(
        state_name = case_when(
            state_code_2001 == 1 ~ "Jammu & Kashimr",
            state_code_2001 == 2 ~ "Himachal Pradesh",
            state_code_2001 == 3 ~ "Punjab",
            state_code_2001 == 4 ~ "Chandigarh",
            state_code_2001 == 5 ~ "Uttarakhand",
            state_code_2001 == 6 ~ "Haryana",
            state_code_2001 == 7 ~ "Delhi",
            state_code_2001 == 8 ~ "Rajasthan",
            state_code_2001 == 9 ~ "Uttar Pradesh",
            state_code_2001 == 10 ~ "Bihar",
            state_code_2001 == 11 ~ "Sikkim",
            state_code_2001 == 12 ~ "Arunachal Pradesh",
            state_code_2001 == 13 ~ "Nagaland",
            state_code_2001 == 14 ~ "Manipur",
            state_code_2001 == 15 ~ "Mizoram",
            state_code_2001 == 16 ~ "Tripura",
            state_code_2001 == 17 ~ "Meghalaya",
            state_code_2001 == 18 ~ "Assam",
            state_code_2001 == 19 ~ "West Bengal",
            state_code_2001 == 20 ~ "Jharkhand",
            state_code_2001 == 21 ~ "Orissa",
            state_code_2001 == 22 ~ "Chhattisgarh",
            state_code_2001 == 23 ~ "Madhya Pradesh",
            state_code_2001 == 24 ~ "Gujarat",
            state_code_2001 == 25 ~ "Daman & Diu",
            state_code_2001 == 26 ~ "Dadra and Nagar Haveli",
            state_code_2001 == 27 ~ "Maharashtra",
            state_code_2001 == 28 ~ "Andhra Pradesh",
            state_code_2001 == 29 ~ "Karnataka",
            state_code_2001 == 30 ~ "Goa",
            state_code_2001 == 31 ~ "Lakshadweep",
            state_code_2001 == 32 ~ "Kerala",
            state_code_2001 == 33 ~ "Tamil Nadu",
            state_code_2001 == 34 ~ "Pondicherry",
            state_code_2001 == 35 ~ "Andaman & Nicobar Islands",
            TRUE ~ NA_character_
        ),
        state_abbr_full = case_when(
            state_name == "Jammu & Kashmir" ~ "JK",
            state_name == "Himachal Pradesh" ~ "HP",
            state_name == "Punjab" ~ "PB",
            state_name == "Chandigarh" ~ "CH",
            state_name == "Uttarakhand" ~ "UK",
            state_name == "Haryana" ~ "HR",
            state_name == "Delhi" ~ "DL",
            state_name == "Rajasthan" ~ "RJ",
            state_name == "Uttar Pradesh" ~ "UP",
            state_name == "Bihar" ~ "BR",
            state_name == "Sikkim" ~ "SK",
            state_name == "Arunachal Pradesh" ~ "AR",
            state_name == "Nagaland" ~ "NL",
            state_name == "Manipur" ~ "MN",
            state_name == "Mizoram" ~ "MZ",
            state_name == "Tripura" ~ "TR",
            state_name == "Meghalaya" ~ "ML",
            state_name == "Assam" ~ "AS",
            state_name == "West Bengal" ~ "WB",
            state_name == "Jharkhand" ~ "JH",
            state_name == "Orissa" ~ "OR",
            state_name == "Chhattisgarh" ~ "CG",
            state_name == "Madhya Pradesh" ~ "MP",
            state_name == "Gujarat" ~ "GJ",
            state_name == "Daman & Diu" ~ "DD",
            state_name == "Dadra & Nagar Haveli" ~ "DD",
            state_name == "Maharashtra" ~ "MH",
            state_name == "Andhra Pradesh" ~ "AP",
            state_name == "Karnataka" ~ "KN",
            state_name == "Goa" ~ "GA",
            state_name == "Lakshadweep" ~ "LD",
            state_name == "Kerala" ~ "KL",
            state_name == "Tamil Nadu" ~ "TN",
            state_name == "Andaman & Nicobar Islands" ~ "AN"
        ),
        government_school = case_when(
            private == 1 ~ 0,
            private == 0 ~ 1,
            TRUE ~ NA_real_
        )
    )

df_src_clean_ihds_years <- df_src %>%
    filter(year == 2011 | year == 2012) %>%
    mutate(
        private = ifelse(sch_management > 96 | sch_management == 4 | sch_management == 5 | sch_management == 8, 1, 0),
        average_absence = ((teachers_non_teaching_days /
            (teachers_male + teachers_female)) / workdays) * 100
    ) %>%
    group_by(state_code_2001, private) %>%
    summarise(absence_dise = mean(average_absence, na.rm = TRUE)) %>%
    mutate(
        state_name = case_when(
            state_code_2001 == 1 ~ "Jammu & Kashimr",
            state_code_2001 == 2 ~ "Himachal Pradesh",
            state_code_2001 == 3 ~ "Punjab",
            state_code_2001 == 4 ~ "Chandigarh",
            state_code_2001 == 5 ~ "Uttarakhand",
            state_code_2001 == 6 ~ "Haryana",
            state_code_2001 == 7 ~ "Delhi",
            state_code_2001 == 8 ~ "Rajasthan",
            state_code_2001 == 9 ~ "Uttar Pradesh",
            state_code_2001 == 10 ~ "Bihar",
            state_code_2001 == 11 ~ "Sikkim",
            state_code_2001 == 12 ~ "Arunachal Pradesh",
            state_code_2001 == 13 ~ "Nagaland",
            state_code_2001 == 14 ~ "Manipur",
            state_code_2001 == 15 ~ "Mizoram",
            state_code_2001 == 16 ~ "Tripura",
            state_code_2001 == 17 ~ "Meghalaya",
            state_code_2001 == 18 ~ "Assam",
            state_code_2001 == 19 ~ "West Bengal",
            state_code_2001 == 20 ~ "Jharkhand",
            state_code_2001 == 21 ~ "Orissa",
            state_code_2001 == 22 ~ "Chhattisgarh",
            state_code_2001 == 23 ~ "Madhya Pradesh",
            state_code_2001 == 24 ~ "Gujarat",
            state_code_2001 == 25 ~ "Daman & Diu",
            state_code_2001 == 26 ~ "Dadra and Nagar Haveli",
            state_code_2001 == 27 ~ "Maharashtra",
            state_code_2001 == 28 ~ "Andhra Pradesh",
            state_code_2001 == 29 ~ "Karnataka",
            state_code_2001 == 30 ~ "Goa",
            state_code_2001 == 31 ~ "Lakshadweep",
            state_code_2001 == 32 ~ "Kerala",
            state_code_2001 == 33 ~ "Tamil Nadu",
            state_code_2001 == 34 ~ "Pondicherry",
            state_code_2001 == 35 ~ "Andaman & Nicobar Islands",
            TRUE ~ NA_character_
        ),
        state_abbr_full = case_when(
            state_name == "Jammu & Kashmir" ~ "JK",
            state_name == "Himachal Pradesh" ~ "HP",
            state_name == "Punjab" ~ "PB",
            state_name == "Chandigarh" ~ "CH",
            state_name == "Uttarakhand" ~ "UK",
            state_name == "Haryana" ~ "HR",
            state_name == "Delhi" ~ "DL",
            state_name == "Rajasthan" ~ "RJ",
            state_name == "Uttar Pradesh" ~ "UP",
            state_name == "Bihar" ~ "BR",
            state_name == "Sikkim" ~ "SK",
            state_name == "Arunachal Pradesh" ~ "AR",
            state_name == "Nagaland" ~ "NL",
            state_name == "Manipur" ~ "MN",
            state_name == "Mizoram" ~ "MZ",
            state_name == "Tripura" ~ "TR",
            state_name == "Meghalaya" ~ "ML",
            state_name == "Assam" ~ "AS",
            state_name == "West Bengal" ~ "WB",
            state_name == "Jharkhand" ~ "JH",
            state_name == "Orissa" ~ "OR",
            state_name == "Chhattisgarh" ~ "CG",
            state_name == "Madhya Pradesh" ~ "MP",
            state_name == "Gujarat" ~ "GJ",
            state_name == "Daman & Diu" ~ "DD",
            state_name == "Dadra & Nagar Haveli" ~ "DD",
            state_name == "Maharashtra" ~ "MH",
            state_name == "Andhra Pradesh" ~ "AP",
            state_name == "Karnataka" ~ "KN",
            state_name == "Goa" ~ "GA",
            state_name == "Lakshadweep" ~ "LD",
            state_name == "Kerala" ~ "KL",
            state_name == "Tamil Nadu" ~ "TN",
            state_name == "Andaman & Nicobar Islands" ~ "AN"
        ),
        government_school = case_when(
            private == 1 ~ 0,
            private == 0 ~ 1,
            TRUE ~ NA_real_
        )
    )
    
# Create Kremer dataset #######################################################

df_kremer <- data.frame(
    state_name = c("Maharashtra", "Gujarat", "Madhya Pradesh", "Kerala",
        "Himachal Pradesh", "Tamil Nadu", "Haryana", "Karnataka", "Orissa",
        "Rajasthan", "West Bengal", "Andhra Pradesh", "Uttar Pradesh",
        "Chhattisgarh", "Uttarakhand", "Assam", "Punjab", "Bihar", "Jharkhand"),
    absence_kremer = c(14.6, 17.0, 17.6, 21.2, 21.2, 21.3, 21.7, 21.7, 23.4,
        23.7, 24.7, 25.3, 26.3, 30.6, 32.8, 33.8, 34.4, 37.8, 41.9),
    state_abbr = c("MH", "GJ", "MP", "KL", "HP", "TN", "HR", "KN", "OR",
        "RJ", "WB", "AP", "UP", "CG", "UK", "AS", "PB", "BR", "JH")
)

# Plotting IHDS and Kramer ####################################################

plot_kremer_ihds <- df_ihds_staff %>%
    filter(government_school == 1) %>%
    select(-government_school) %>%
    filter(
        state_name == "Maharashtra" | state_name == "Gujarat" |
        state_name == "Madhya Pradesh" | state_name == "Kerala" |
        state_name == "Himachal Pradesh" | state_name == "Tamil Nadu" |
        state_name == "Haryana" | state_name == "Karnataka" |
        state_name == "Orissa" | state_name == "Rajasthan" |
        state_name == "West Bengal" | state_name == "Andhra Pradesh" |
        state_name == "Uttar Pradesh" | state_name == "Chhattisgarh" |
        state_name == "Uttarakhand" | state_name == "Assam" |
        state_name == "Punjab" | state_name == "Bihar" |
        state_name == "Jharkhand"
    ) %>%
    left_join(., df_kremer, by = "state_name") %>%
    ggplot(aes(x = absence_ihds, y = absence_kremer, label = state_abbr)) +
    geom_text(size = 2.5) +
    geom_smooth(method = lm, se = FALSE, colour = single_colour) +
    theme_bw() +
    labs(
        x = "Absence: IHDS (%)",
        y = "Absence: World Bank (%)",
        subtitle = "B"
    ) +
    theme(
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8)
    )

df_src_kremer_nowb <- df_src_clean %>%
    filter(private == 0) %>%
    select(-private) %>%
    filter(
        state_code_2001 == 2 | state_code_2001 == 3 | state_code_2001 == 5 |
        state_code_2001 == 6 | state_code_2001 == 8 | state_code_2001 == 9 |
        state_code_2001 == 10 | state_code_2001 == 18 | state_code_2001 == 19 |
        state_code_2001 == 20 | state_code_2001 == 21 | state_code_2001 == 22 |
        state_code_2001 == 23 | state_code_2001 == 24 | state_code_2001 == 27 |
        state_code_2001 == 28 | state_code_2001 == 29 | state_code_2001 == 32 |
        state_code_2001 == 33
    ) %>%
    left_join(., df_kremer, by = "state_name") %>%
    filter(state_name != "West Bengal")

df_src_kremer <- df_src_clean %>%
    filter(private == 0) %>%
    select(-private) %>%
    filter(
        state_code_2001 == 2 | state_code_2001 == 3 | state_code_2001 == 5 |
        state_code_2001 == 6 | state_code_2001 == 8 | state_code_2001 == 9 |
        state_code_2001 == 10 | state_code_2001 == 18 | state_code_2001 == 19 |
        state_code_2001 == 20 | state_code_2001 == 21 | state_code_2001 == 22 |
        state_code_2001 == 23 | state_code_2001 == 24 | state_code_2001 == 27 |
        state_code_2001 == 28 | state_code_2001 == 29 | state_code_2001 == 32 |
        state_code_2001 == 33
    ) %>%
    left_join(., df_kremer, by = "state_name")

plot_dise_kremer <- df_src_kremer %>%
    ggplot(aes(x = absence_dise, y = absence_kremer, label = state_abbr)) +
    geom_text(size = 2.5) +
    geom_smooth(method = lm, se = FALSE, aes(color = "Full Sample")) +
    geom_smooth(
        data = df_src_kremer_nowb,
        aes(x = absence_dise, y = absence_kremer, color = "Excluding WB"),
        method = lm,
        se = FALSE
    ) +
    theme_bw() +
    labs(
        x = "Absence: DISE (%)",
        y = "Absence: World Bank (%)",
        subtitle = "A"
    ) +
    theme(
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8)
    ) +
    scale_colour_manual(values = c(two_colours))

df_ihds_gov <- df_ihds_staff %>% filter(government_school == 1)

df_src_ihds_nowb <- df_src_clean %>%
    filter(private == 0) %>%
    select(-private) %>%
    left_join(., df_ihds_gov, by = "state_name") %>%
    filter(state_name != "West Bengal" & state_name != "Goa" &
        state_name != "Meghalaya" & state_name != "Nagaland"
    )

plot_dise_ihds <- df_src_clean %>%
    filter(government_school == 1) %>%
    select(-c(private, government_school)) %>%
    left_join(., df_ihds_gov, by = "state_name") %>%
    filter(
        state_name != "Goa" & state_name != "Meghalaya" &
        state_name != "Nagaland"
    ) %>%
    ggplot(aes(x = absence_dise, y = absence_ihds, label = state_abbr_full)) +
    geom_text(size = 2.5) +
    geom_smooth(method = lm, se = FALSE, aes(colour = "Full Sample")) +
    geom_smooth(
        data = df_src_ihds_nowb,
        aes(x = absence_dise, y = absence_ihds, color = "Excluding WB"),
        method = lm,
        se = FALSE
    ) +
    theme_bw() +
    labs(
        x = "Absence: DISE (%)",
        y = "Absence: IHDS (%)",
        subtitle = "C"
    ) +
    theme(
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8)
    ) +
    scale_colour_manual(values = c(two_colours))

plot_dise_ihds_years <- df_src_clean_ihds_years %>%
    filter(government_school == 1) %>%
    select(-c(private, government_school)) %>%
    left_join(., df_ihds_gov, by = "state_name") %>%
    ggplot(aes(x = absence_dise, y = absence_ihds, label = state_abbr_full)) +
    geom_text() +
    geom_smooth(method = lm, se = FALSE, aes(color = "Full Sample")) +
    geom_smooth(
        data = df_src_ihds_nowb,
        aes(x = absence_dise, y = absence_ihds, color = "Excluding WB"),
        method = lm,
        se = FALSE
    ) +
    theme_bw() +
    labs(
        x = "Absence: DISE (%)",
        y = "Absence: IHDS (%)"
    ) +
    theme(
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)
    ) +
    scale_colour_manual(values = c(two_colours))

plot_data_compare <- (plot_dise_kremer + plot_kremer_ihds + plot_dise_ihds)

plot_data_compare

ggsave(
    here("data/output/figures/absencecomparison.pdf"),
    units = "in",
    width = 6.5,
    height = 4.5
)
