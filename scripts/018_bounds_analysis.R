# Metadata ####################################################################
# Name: 018_bounds_analysis.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Re-runs main analysis bounding effect sizes

# Load packages ###############################################################

library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(broom)
library(dotwhisker)
library(data.table)
library(fixest)
library(future.apply)
library(rio)
library(here)

# Loading data ################################################################

df_src <- readRDS(here("data/clean/absence_src_clean.rds"))
setDT(df_src)
cols_keep <- c(
    "private", "vs_year_0", "vs_year_1", "vs_year_2", "vs_year_3", "vs_year_4",
    "absent", "ihs_average_absence", "rural", "lag_absent",
    "lag_ihs_average_absence", "teachers", "year", "school_code",
    "unique_constituency_code"
)
df_src_government <- df_src[private == 0, ..cols_keep]
rm(df_src)

# Setting up locals for run ###################################################

n_runs <- 200
list_coefficients <- vector(mode = "list", length = n_runs)
sample_number <- seq(from = 10, to = 100, by = 10)
unique_constituencies <- unique(df_src_government[, unique_constituency_code])

# Constituency Bounding Analysis with Dummies #################################
# Replace all the schools in one constituency in an election year to be absent

for(samples in sample_number) {

    for(run in 1:n_runs) {
        print(paste0("Run #", run, " while taking ", samples, " samples for the run replacing the dummies"))
        set.seed(run)
        start_time <- Sys.time()

        sample_constituencies <- sample(unique_constituencies, samples)

        df_src_government[, new_absent := fifelse(
                vs_year_1 == 1 & (unique_constituency_code %in% c(sample_constituencies)),
                1,
                absent)
            ]

        lm <- feols(
            new_absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural + lag_absent
                + teachers,
            cluster = c("unique_constituency_code", "year"),
            fixef = c("year", "school_code"),
            data = df_src_government
        )

        list_coefficients[[run]][["mean"]] <- lm$coefficients[[2]]
        list_coefficients[[run]][["se"]] <- lm$se[[2]]
        end_time <- Sys.time()

        print(end_time - start_time)

    }

    df <- do.call(rbind.data.frame, list_coefficients)
    colnames(df)[1] <- paste0("point_estimate_", samples)
    colnames(df)[2] <- paste0("se_", samples)

    export(
        df,
        file.path(
            here(), paste0("data/clean/constituency_bounds_", samples, ".csv")
        )
    )
}

# Constituency Bounding Analysis at the school-level median ###################

df_src_government[, school_level_median := median(ihs_average_absence, na.rm = TRUE), by = "school_code"]

for(samples in sample_number) {

    for(run in 1:n_runs) {
        print(paste0("Run #", run, " while taking ", samples, " samples for the run setting values at the school-level median"))
        median_run <- run + 200
        set.seed(median_run)
        start_time <- Sys.time()

        sample_constituencies <- sample(unique_constituencies, samples)

        df_src_government[, new_absent := fifelse(
                vs_year_1 == 1 & (unique_constituency_code %in% c(sample_constituencies)),
                school_level_median,
                ihs_average_absence)
            ]

        lm <- feols(
            new_absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
                lag_ihs_average_absence + teachers,
            cluster = c("unique_constituency_code", "year"),
            fixef = c("year", "school_code"),
            data = df_src_government
        )

        list_coefficients[[run]][["mean"]] <- lm$coefficients[[2]]
        list_coefficients[[run]][["se"]] <- lm$se[[2]]
        end_time <- Sys.time()

        print(end_time - start_time)

    }

    df <- do.call(rbind.data.frame, list_coefficients)
    colnames(df)[1] <- paste0("point_estimate_median_", samples)
    colnames(df)[2] <- paste0("se_median_", samples)

    export(
        df,
        file.path(
            here(), paste0("data/clean/constituency_bounds_median_", samples, ".csv")
        )
    )
}

# Constituency Bounding Analysis at the school-level maximum ##################

df_src_government[, school_level_max := max(ihs_average_absence, na.rm = TRUE), by = "school_code"]

for(samples in sample_number) {

    for(run in 1:n_runs) {
        print(paste0("Run #", run, " while taking ", samples, " samples for the run setting values at the school-level max"))
        max_run <- run + 400
        set.seed(max_run)
        start_time <- Sys.time()

        sample_constituencies <- sample(unique_constituencies, samples)

        df_src_government[, new_absent := fifelse(
                vs_year_1 == 1 & (unique_constituency_code %in% c(sample_constituencies)),
                school_level_max,
                ihs_average_absence)
            ]

        lm <- feols(
            new_absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
                lag_ihs_average_absence + teachers,
            cluster = c("unique_constituency_code", "year"),
            fixef = c("year", "school_code"),
            data = df_src_government
        )

        list_coefficients[[run]][["mean"]] <- lm$coefficients[[2]]
        list_coefficients[[run]][["se"]] <- lm$se[[2]]
        end_time <- Sys.time()

        print(end_time - start_time)

    }

    df <- do.call(rbind.data.frame, list_coefficients)
    colnames(df)[1] <- paste0("point_estimate_max_", samples)
    colnames(df)[2] <- paste0("se_max_", samples)

    export(
        df,
        file.path(
            here(), paste0("data/clean/constituency_bounds_max_", samples, ".csv")
        )
    )
}

# Producing bounds figure #####################################################

# Dummy bounds ################################################################

list_bounds <- future_lapply(seq.int(10, 100, 10), function(x) import(file.path(here(), paste0("data/clean/constituency_bounds_", x, ".csv"))))
df_bounds <- bind_cols(list_bounds)
df_means <- as.data.frame(t(as.matrix(colMeans(df_bounds))))

df_means_point_estimate <- as.data.frame(t(as.matrix(colMeans(df_bounds[, grepl("point_estimate_", colnames(df_bounds))]))))

df_means_point_estimate <- pivot_longer(
    df_means_point_estimate,
    cols = everything(),
    names_to = "term"
) %>%
    rename(estimate = value) %>%
    mutate(term = str_replace(term, "point_estimate_", ""))

df_means_se <- as.data.frame(t(as.matrix(colMeans(df_bounds[, grepl("se_", colnames(df_bounds))]))))

df_means_se <- pivot_longer(
    df_means_se,
    cols = everything(),
    names_to = "term"
) %>%
    rename(std.error = value) %>%
    mutate(term = str_replace(term, "se_", ""))

df_means_se <- df_means_se %>%
    left_join(., df_means_point_estimate, by = "term")

lm <- feols(
    absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
    lag_absent + teachers,
    cluster = c("unique_constituency_code", "year"),
    fixef = c("year", "school_code"),
    data = df_src_government
)

df_plot <- tidy(lm) %>%
    select(term, estimate, std.error) %>%
    filter(term == "vs_year_1") %>%
    mutate(term = "Original\nEstimate") %>%
    rbind(., df_means_se) %>%
    mutate(colour = as.factor(ifelse(term == "Original\nEstimate", 1, 0)))

plot_placebo_test_dummy <- dwplot(
        df_plot,
        conf.level = .95,
        vline = geom_vline(xintercept = 0, linetype = 2)
    ) +
    theme_bw() +
    scale_colour_manual(values = c("1" = "red", "0" = "black")) +
    geom_segment(
        aes(
            x = estimate - (1.96 * std.error),
            y = term,
            xend = estimate + (1.96 * std.error),
            yend = term,
            col = colour
        )
    ) +
    geom_point(aes(x = estimate, y = term, col = colour)) +
    labs(
        y = "Number of Constituencies Increased Absence",
        x = "Point Estimate",
        color = NULL
    ) +
    theme(
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "transparent",
            color = NA),
    )

# Median bounds ###############################################################

list_bounds <- future_lapply(seq.int(10, 100, 10), function(x) import(file.path(here(), paste0("data/clean/constituency_bounds_median_", x, ".csv"))))
df_bounds <- bind_cols(list_bounds)
df_means <- as.data.frame(t(as.matrix(colMeans(df_bounds))))

df_means_point_estimate <- as.data.frame(t(as.matrix(colMeans(df_bounds[, grepl("point_estimate_", colnames(df_bounds))]))))

df_means_point_estimate <- pivot_longer(
    df_means_point_estimate,
    cols = everything(),
    names_to = "term"
) %>%
    rename(estimate = value) %>%
    mutate(term = str_replace(term, "point_estimate_median_", ""))

df_means_se <- as.data.frame(t(as.matrix(colMeans(df_bounds[, grepl("se_", colnames(df_bounds))]))))

df_means_se <- pivot_longer(
    df_means_se,
    cols = everything(),
    names_to = "term"
) %>%
    rename(std.error = value) %>%
    mutate(term = str_replace(term, "se_median_", ""))

df_means_se <- df_means_se %>%
    left_join(., df_means_point_estimate, by = "term")

lm <- feols(
    ihs_average_absence ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
        lag_ihs_average_absence + teachers,
    cluster = c("unique_constituency_code", "year"),
    fixef = c("year", "school_code"),
    data = df_src_government
)

df_plot <- tidy(lm) %>%
    select(term, estimate, std.error) %>%
    filter(term == "vs_year_1") %>%
    mutate(term = "Original\nEstimate") %>%
    rbind(., df_means_se) %>%
    mutate(colour = as.factor(ifelse(term == "Original\nEstimate", 1, 0)))

plot_placebo_test_median <- dwplot(
        df_plot,
        conf.level = .95,
        vline = geom_vline(xintercept = 0, linetype = 2)
    ) +
    theme_bw() +
    scale_colour_manual(values = c("1" = "red", "0" = "black")) +
    geom_segment(
        aes(
            x = estimate - (1.96 * std.error),
            y = term,
            xend = estimate + (1.96 * std.error),
            yend = term,
            col = colour
        )
    ) +
    geom_point(aes(x = estimate, y = term, col = colour)) +
    labs(
        y = "Number of Constituencies Increased Absence",
        x = "Point Estimate",
        color = NULL
    ) +
    theme(
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "transparent",
            color = NA),
    )

# Max Bounds #################################################################

list_bounds <- future_lapply(seq.int(10, 100, 10), function(x) import(file.path(here(), paste0("data/clean/constituency_bounds_max_", x, ".csv"))))
df_bounds <- bind_cols(list_bounds)
df_means <- as.data.frame(t(as.matrix(colMeans(df_bounds))))

df_means_point_estimate <- as.data.frame(t(as.matrix(colMeans(df_bounds[, grepl("point_estimate_", colnames(df_bounds))]))))

df_means_point_estimate <- pivot_longer(
    df_means_point_estimate,
    cols = everything(),
    names_to = "term"
) %>%
    rename(estimate = value) %>%
    mutate(term = str_replace(term, "point_estimate_max_", ""))

df_means_se <- as.data.frame(t(as.matrix(colMeans(df_bounds[, grepl("se_", colnames(df_bounds))]))))

df_means_se <- pivot_longer(
    df_means_se,
    cols = everything(),
    names_to = "term"
) %>%
    rename(std.error = value) %>%
    mutate(term = str_replace(term, "se_max_", ""))

df_means_se <- df_means_se %>%
    left_join(., df_means_point_estimate, by = "term")

df_plot <- tidy(lm) %>%
    select(term, estimate, std.error) %>%
    filter(term == "vs_year_1") %>%
    mutate(term = "Original\nEstimate") %>%
    rbind(., df_means_se) %>%
    mutate(colour = as.factor(ifelse(term == "Original\nEstimate", 1, 0)))

plot_placebo_test_max <- dwplot(
        df_plot,
        conf.level = .95,
        vline = geom_vline(xintercept = 0, linetype = 2)
    ) +
    theme_bw() +
    scale_colour_manual(values = c("1" = "red", "0" = "black")) +
    geom_segment(
        aes(
            x = estimate - (1.96 * std.error),
            y = term,
            xend = estimate + (1.96 * std.error),
            yend = term,
            col = colour
        )
    ) +
    geom_point(aes(x = estimate, y = term, col = colour)) +
    labs(
        y = "Number of Constituencies Increased Absence",
        x = "Point Estimate",
        color = NULL
    ) +
    theme(
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "transparent",
            color = NA),
    )

# Putting plots together ######################################################

plots <- list(
    plot_placebo_test_dummy, plot_placebo_test_median, plot_placebo_test_max
)

plot <- wrap_plots(plots, ncol = 3, axes = "collect") +
    plot_layout(axes = "collect_y") +
    plot_annotation(tag_levels = 'A')

ggsave(
    filename = file.path(here(), "data/output/figures/placebotest.pdf"),
    plot,
    height = 4.5,
    width = 6.5,
    units = "in"
)

ggsave(
    filename = file.path(here(), "data/output/figures/placebotest.tiff"),
    plot,
    height = 4.5,
    width = 6.5,
    units = "in"
)
