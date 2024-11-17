###############################################################################
# Name: 013_absence_cost.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Calculates back of the envelope calculations of fiscal cost of
# absenteeism as well as total teaching days lost
# Date Created: 2021/06/15
###############################################################################

###############################################################################
# Loading packages
###############################################################################

library(lfe)
library(xtable)
library(dplyr)
library(here)

df_src <- readRDS(here::here("data/clean/absence_src_clean.rds")) %>%
    filter(private == 0)

###############################################################################
# Inputting wages
###############################################################################

lowest_wage <- 5418 # from Kingdon and Sipahimalani-Rao 2010 pg. 61
highest_wage <- 11368 # from Muralidharan et al. 2017 pg. 126
daily_low <- lowest_wage / 25 # Assume 25 working days per month -- 20 week days + Saturday
daily_high <- highest_wage / 25 # Assume 25 working days per month -- 20 week days + Saturday
exchange_rate <- 46 # Yahoo finance on 01/01/2010: https://finance.yahoo.com/quote/USDINR=x

###############################################################################
# Calculating point estimate from highest to lowest 
###############################################################################

log_year_school_fe <- felm(
    ihs_average_absence ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 +
    teachers + rural + lag_ihs_average_absence | factor(school_code) +
    factor(year) | 0 | school_code + year,
    data = df_src,
    cmethod = "reghdfe")
 
# Extracting point estimate on greatest difference:
absence_reduction <- 10^(log_year_school_fe$coefficients[3,] - log_year_school_fe$coefficients[2,])

# Calculating total number of teachers:
df_year <- df_src %>%
    group_by(year) %>%
    summarise(teachers = sum(teachers, na.rm = TRUE))

number_teachers <- mean(df_year$teachers)
mean_absence <- mean(df_src$teachers_non_teaching_days[df_src$private == 0], na.rm = TRUE)

# Calculating Average level of wages lost as a result of absenteeism
total_wage_loss_low <- (daily_low * mean_absence) * number_teachers
total_wage_loss_high <- (daily_high * mean_absence) * number_teachers

# Calculating how much wages were recovered from reduced absenteeism
wage_recovery_low <- (daily_low * absence_reduction) * number_teachers
wage_recovery_high <- (daily_high * absence_reduction) * number_teachers

# Calculating total wages lost and recovered in dollars
wage_loss_dollars_low <- total_wage_loss_low / exchange_rate
wage_loss_dollars_high <- total_wage_loss_high / exchange_rate
wage_recovery_dollars_low <- wage_recovery_low / exchange_rate
wage_recovery_dollars_high <- wage_recovery_high / exchange_rate

# Exporting Mean absence:
sink(file = here("data/output/text/meanabsence.tex"))
cat(round(mean_absence, 2))
sink(file = NULL)

# Exporting Total Number of Teachers:
sink(file = here("data/output/text/teachers.tex"))
cat(format(round(number_teachers, 0), big.mark = ",", trim = TRUE))
sink(file = NULL)

# Calculating the share of the budget lost to absence recovered in election years:
share_budget <- (wage_recovery_high /
    total_wage_loss_high) * 100
sink(file = here("data/output/text/sharebudget.tex"))
cat(round(share_budget, 0))
sink(file = NULL)

# Exporting highest and lowest wage estimates:
sink(file = here("data/output/text/highwage.tex"))
cat(format(highest_wage, big.mark = ",", trim = TRUE))
sink(file = NULL)

sink(file = here("data/output/text/lowwage.tex"))
cat(format(lowest_wage, big.mark = ",", trim = TRUE))
sink(file = NULL)

# Exporting highest and lowest daily wage estimates:
sink(file = here("data/output/text/dailyhigh.tex"))
cat(format(daily_high, big.mark = ",", trim = TRUE))
sink(file = NULL)

sink(file = here("data/output/text/dailylow.tex"))
cat(format(daily_low, big.mark = ",", trim = TRUE))
sink(file = NULL)

# Exporting total wage loss estimates
sink(file = here("data/output/text/wagelosslow.tex"))
cat(format(total_wage_loss_low, big.mark = ",", trim = TRUE))
sink(file = NULL)

sink(file = here("data/output/text/wagelosshigh.tex"))
cat(format(total_wage_loss_high, big.mark = ",", trim = TRUE))
sink(file = NULL)

# Exporting total wage loss estimates in Dollars
sink(file = here("data/output/text/wagelossdollarslow.tex"))
cat(format(wage_loss_dollars_low, big.mark = ",", trim = TRUE))
sink(file = NULL)

sink(file = here("data/output/text/wagelossdollarshigh.tex"))
cat(format(wage_loss_dollars_high, big.mark = ",", trim = TRUE))
sink(file = NULL)

# Exporting wage recovery estimates
sink(file = here("data/output/text/wagerecoverylow.tex"))
cat(format(wage_recovery_low, big.mark = ",", trim = TRUE))
sink(file = NULL)

sink(file = here("data/output/text/wagerecoveryhigh.tex"))
cat(format(wage_recovery_high, big.mark = ",", trim = TRUE))
sink(file = NULL)

# Exporting wage recovery estimates in dollars
sink(file = here("data/output/text/wagerecoverylowdollar.tex"))
cat(format(wage_recovery_dollars_low, big.mark = ",", trim = TRUE))
sink(file = NULL)

sink(file = here("data/output/text/wagerecoveryhighdollar.tex"))
cat(format(wage_recovery_dollars_high, big.mark = ",", trim = TRUE))
sink(file = NULL)

# Exporting absence reduction:
sink(file = here("data/output/text/absencereduction.tex"))
cat(round(absence_reduction, 2))
sink(file = NULL)


