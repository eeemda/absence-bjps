###############################################################################
# Name: 011_match_differences.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Checks for differences on observables for schools that were unmatched
# in data
# Date created: 2021/06/08
###############################################################################

###############################################################################
# Loading packages
###############################################################################

library(arrow)
library(data.table)
library(dplyr)
library(stringr)
library(broom)
library(dotwhisker)
library(here)

###############################################################################
# Loading data
###############################################################################

df_src_original <- read_parquet(here("data/clean/src_matching.parquet"))

df_src <- df_src_original %>%
    mutate(matched = ifelse(merge_constituency != "No Merge", 1, 0)) %>%
    select(-merge_constituency)

setDT(df_src)

# Keeping variables we will use for analysis
keep_variables_school_level <- c("school_code", "matched", "state_name",
    "rural", "private")
keep_variables_school_year_level <- c("matched", "teachers", "average_absence",
    "year", "absent", "administrative_visits", "smc_meetings")

df_src_school_level <- df_src[, ..keep_variables_school_level]
df_src <- df_src[, ..keep_variables_school_year_level]

df_src_school_level <- df_src_school_level %>%
    distinct(school_code, .keep_all = TRUE) %>%
    mutate(
        state_name = str_replace(state_name, "&", ""),
        state_name = str_replace_all(state_name, " ", ""),
        state_name = ifelse(state_name %in% NA, "missing", state_name),
        state_name = as.factor(state_name),
        rural = case_when(
            rural == "Urban" ~ 0,
            rural == "Rural" ~ 1
        )
    ) %>%
    select(-school_code)

df_src <- df_src %>%
    mutate(
        year = as.factor(year),
        teachers = (teachers - mean(teachers, na.rm = TRUE)) / (2 * sd(teachers, na.rm = TRUE)),
        average_absence = (average_absence -
            mean(average_absence, na.rm = TRUE)) /
            (2 * sd(average_absence, na.rm = TRUE)),
        administrative_visits = (administrative_visits -
            mean(administrative_visits, na.rm = TRUE)) /
            (2 * sd(administrative_visits, na.rm = TRUE)),
        smc_meetings = (smc_meetings - mean(smc_meetings, na.rm = TRUE)) /
            (2 * sd(smc_meetings, na.rm = TRUE))
    )

# Convert states into model matrix
state_dummies <- model.matrix(~ state_name - 1, df_src_school_level)
year_dummies <- model.matrix(~ year - 1, df_src)

df_src_school_level <- cbind(df_src_school_level, state_dummies) %>%
    select(-state_name)

df_src <- cbind(df_src, year_dummies) %>%
    select(-year)

###############################################################################
# Run regressions on matching
###############################################################################

# Extracting names of all dependent variables
list_dvs_src <- names(df_src)
list_dvs_src <- list_dvs_src[list_dvs_src != "matched"]

list_dvs_src_school_level <- names(df_src_school_level)
list_dvs_src_school_level <- list_dvs_src_school_level[list_dvs_src_school_level != "matched"]

# Function to run models
modelFunction <- function(dependent_variable, data_frame) {
    print(paste0("Running regression on ", dependent_variable))
    linear_model <- lm(paste0(dependent_variable, " ~ matched"),
        data = data_frame
    )
    linear_model <- tidy(linear_model)
    linear_model <- linear_model %>%
        filter(term == "matched") %>%
        mutate(term = dependent_variable)
    return(linear_model)
}

# Running model over entire dataset
school_level_models <- lapply(list_dvs_src_school_level, modelFunction, data_frame = df_src_school_level) %>%
    bind_rows()

year_level_models <- lapply(list_dvs_src, modelFunction, data_frame = df_src) %>%
    bind_rows()    

list_models <- bind_rows(school_level_models, year_level_models) %>%
    mutate(
        term = str_replace(term, "state_name", ""),
        term = str_replace(term, "_", " "),
        term = str_replace(term, "\\B[B]\\B", " B"),
        term = str_replace(term, "\\B[P]\\B", " P"),
        term = str_replace(term, "\\B[K]\\B", " & K"),
        term = str_replace(term, "\\B[N]\\B", " N"),
        term = str_replace(term, "missing", "State Missing"),
        term = str_replace(term, "year", "Year: "),
        term = str_to_title(term),
        term = str_replace(term, "Smc", "SMC")        
    ) %>%
    arrange(-estimate)

# Plot and export test of differences between matched and unmatched schools
plot_matching_test <- dwplot(list_models,
    show_intercept = TRUE,
    vline = geom_vline(xintercept = 0, linetype = 2)) +
    theme_bw() +
    theme(
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
    ) +
    labs(x = "% Difference")

ggsave(here("data/output/figures/matchingratetest.pdf"),
    height = 8,
    width = 6.5,
    units = "in",
    bg = "transparent"
)

ggsave(here("data/output/figures/matchingratetest.tiff"),
    height = 8,
    width = 6.5,
    units = "in",
    bg = "transparent"
)
