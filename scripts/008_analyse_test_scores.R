# Metadata ####################################################################
# Name: 008_analyse_test_scores.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Date created: 2020/04/01
# Purpose: Run electoral cycle models on IHDS test score data

# Load Packages ###############################################################

library(broom)
library(ggplot2)
library(dotwhisker)
library(lfe)
library(sandwich)
library(modelsummary)
library(tinytable)
library(dplyr)
library(stringr)
library(viridis)
library(patchwork)
library(here)

# Load data ###################################################################

load(file = here("data/clean/ihds_student.RData"))
load(file = here("data/clean/ihds_schools_elections.RData"))

# Merging election dates with student data ####################################

df_ihds_village <- df_ihds_school_clean %>%
    select(id_psu, years_to_election_no_by, state_district) %>%
    rename(years_to_election = years_to_election_no_by) %>%
    distinct()

df_ihds_student <- left_join(df_ihds_student_clean, df_ihds_village,
    by = c("id_psu"))

# Linear models of test scores in goverment schools ###########################

lm_overall <- felm(
    score_total ~
    years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 0)
)
cov <- vcovHC(lm_overall, type = "HC1")
se_lm_overall <- sqrt(diag(cov))
df_lm_overall <- tidy(lm_overall) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Overall",
        plot_order = 1
    )

lm_reading <- felm(
    score_reading ~
    years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 0)
)
cov <- vcovHC(lm_reading, type = "HC1")
se_lm_reading <- sqrt(diag(cov))
df_lm_reading <- tidy(lm_reading) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Reading",
        plot_order = 2
    )

lm_math <- felm(
    score_math ~ years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 0)
)
cov <- vcovHC(lm_math, type = "HC1")
se_lm_math <- sqrt(diag(cov))
df_lm_math <- tidy(lm_math) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Math",
        plot_order = 3
    )

lm_writing <- felm(
    score_writing ~ years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 0)
)
cov <- vcovHC(lm_writing, type = "HC1")
se_lm_writing <- sqrt(diag(cov))
df_lm_writing <- tidy(lm_writing) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Writing",
        plot_order = 4
    )

df_lm_test_scores <- bind_rows(
        df_lm_overall, df_lm_reading, df_lm_math, df_lm_writing
    ) %>%
    filter(str_detect(term, "election")) %>%
    mutate(
        term = ifelse(term == "years_to_election-2", "-2 or more", term),
        term = ifelse(term == "years_to_election2", "2 or more", term),
        term = gsub("years_to_election", "", term),
        order = ifelse(term == "-2 or more", 5, 0),
        order = ifelse(term == "-1", 4, order),
        order = ifelse(term == "1", 2, order),
        order = ifelse(term == "2 or more", 1, order),
        order = ifelse(term == "Election Year", 3, order)
    ) %>%
    arrange(plot_order, order) %>%
    select(-plot_order, order)

plot_test_scores <- dwplot(df_lm_test_scores,
    conf.level = .95,
    show_intercept = TRUE,
    vline = geom_vline(xintercept = 0, linetype = 2)) +
    theme_bw() +
    scale_color_viridis_d() +
    labs(color = NULL) +
    theme(
        legend.position = "none",
        # Removing x tick marks
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
        panel.grid.major.x = element_blank()
    ) +
    coord_flip()

# Linear models of test scores in private schools #############################

lm_overall_private <- felm(score_total ~
    years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 1))
cov <- vcovHC(lm_overall, type = "HC1")
se_lm_overall_private <- sqrt(diag(cov))
df_lm_overall <- tidy(lm_overall_private) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Overall",
        plot_order = 1
    )

lm_reading_private <- felm(score_reading ~
    years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 1))
cov <- vcovHC(lm_reading, type = "HC1")
se_lm_reading_private <- sqrt(diag(cov))
df_lm_reading <- tidy(lm_reading_private) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Reading",
        plot_order = 2
    )

lm_math_private <- felm(score_math ~
    years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 1))
cov <- vcovHC(lm_math, type = "HC1")
se_lm_math_private <- sqrt(diag(cov))
df_lm_math <- tidy(lm_math_private) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Math",
        plot_order = 3
    )

lm_writing_private <- felm(score_writing ~
    years_to_election + male + age + class + teacher_local,
    data = subset(df_ihds_student, private == 1))
cov <- vcovHC(lm_writing, type = "HC1")
se_lm_writing_private <- sqrt(diag(cov))
df_lm_writing <- tidy(lm_writing_private) %>%
    add_row(
        term = "Election Year", estimate = 0, std.error = 0, statistic = 0,
        p.value = 0
    ) %>%
    mutate(
        model = "Writing",
        plot_order = 4
    )

df_lm_test_scores_private <- bind_rows(
        df_lm_overall, df_lm_reading, df_lm_math, df_lm_writing
    ) %>%
    filter(str_detect(term, "lection")) %>%
    mutate(
        term = ifelse(term == "years_to_election-2", "-2 or more", term),
        term = ifelse(term == "years_to_election2", "2 or more", term),
        term = gsub("years_to_election", "", term),
        order = ifelse(term == "-2 or more", 5, 0),
        order = ifelse(term == "-1", 4, order),
        order = ifelse(term == "1", 2, order),
        order = ifelse(term == "2 or more", 1, order),
        order = ifelse(term == "Election Year", 3, order)
    ) %>%
    arrange(plot_order, order) %>%
    select(-plot_order, order)

plot_test_scores_private <- dwplot(
    df_lm_test_scores_private,
    conf.level = .95,
    show_intercept = TRUE,
    vline = geom_vline(xintercept = 0, linetype = 2)) +
    theme_bw() +
    scale_color_viridis_d() +
    labs(y = "Years to Election", color = NULL) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.922, 0.75),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.key = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
        panel.grid.major.x = element_blank()
    ) +
    coord_flip()

plot_test_scores_private_presentation <- dwplot(
    df_lm_test_scores_private,
    conf.level = .95,
    show_intercept = TRUE,
    vline = geom_vline(xintercept = 0, linetype = 2)) +
    theme_bw() +
    scale_color_viridis_d() +
    labs(
        y = "Years to Election",
        color = NULL
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.922, 0.26),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.key = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent",
            color = NA), # bg of the plot
        panel.grid.major.x = element_blank()
    ) +
    coord_flip()

plots <- list(plot_test_scores, plot_test_scores_private)

plot <- wrap_plots(plots, ncol = 1, axes = "collect") +
    plot_layout(axes = "collect_x") +
    plot_annotation(tag_levels = "A")

ggsave(
    here("data/output/figures/testscorecycle.pdf"),
    height = 4.5,
    width = 6.5,
    units = "in",
    bg = "transparent"
)

models <- list(
    "Overall Score" = lm_overall,
    "Reading" = lm_reading,
    "Math" = lm_math,
    "Writing" = lm_writing,
    "Overall Score" = lm_overall_private,
    "Reading" = lm_reading_private,
    "Math" = lm_math_private,
    "Writing" = lm_writing_private
)

f <- function(x) format(round(x, 3), big.mark = ",")
gof <- list(list("raw" = "nobs", "clean" = "Observations", "fmt" = f))
table_note <- paste0(
        "\\emph{Notes}: * p < 0.1, ** p < 0.05, *** p < 0.01. Robust standard errors are in parentheses. I run results for test scores overall, and four reading, math, and writing comprehension separately.  The overall scores are a sum of the other three scores and each score is rescaled from 0 to 1.  For reading, a child is scored as unable to read, able to read letters, words, paragraphs, or able to read an entire story. In math, a child is scored as unable to recognize a number, whether they can recognize a number, whether they can subtract one-digit numbers, or whether they can divide a two-digit number by a one-digit number.  For writing, a child is scored by whether they cannot write, can write a paragraph with two mistakes or fewer, or can write with no mistakes. This presents the results from Figure \ref{figure:testscorecycle}."
    )

tab <- modelsummary(
    models,
    title = "Test Scores Improve in Election Years Relative to Non-Election Years\\label{table:testscorecycle}",
    coef_map = c(
        "years_to_election-2" = "2 or More Years Before Election",
        "years_to_election-1" = "1 Year Before Election",
        "years_to_election1" = "1 Year After Election",
        "years_to_election2" = "2 or More Years After Election",
        "male" = "Male",
        "age" = "Age",
        "class" = "Class",
        "teacher_local" = "Local Teacher",
        "(Intercept)" = "Election Year"
    ),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    estimate = "{estimate}{stars}",
    gof_map = gof,
    notes = table_note,
    output = "tinytable",
    escape = FALSE
    ) %>%
    group_tt(j = list("Government Schools" = 2:5, "Private Schools" = 6:9)) %>%
    style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
    save_tt(here("data/output/tables/ihdstestscores.tex"), overwrite = TRUE)
