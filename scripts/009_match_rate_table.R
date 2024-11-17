###############################################################################
# Name: 009_match_rate_table.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Creates a matching rate table that describes how many matches by
# matching strategy
# Date created: 2021/05/11
###############################################################################

###############################################################################
# Loading packages
###############################################################################

library(arrow)
library(xtable)
library(dplyr)
library(here)

###############################################################################
# Loading data
###############################################################################

df_src <- read_parquet(here("data/clean/src_matching.parquet"))

###############################################################################
# Creating match rate table
###############################################################################

# Getting unique school ids:
df_src <- df_src %>% distinct(school_code, .keep_all = TRUE)

# Total schools by school type
n_government <- nrow(df_src[df_src$private == 0, ])
n_private <- as.numeric(nrow(df_src[df_src$private == 1, ]))

# Matched by GIS locations:
matched_gis_government <- as.numeric(nrow(df_src[df_src$private == 0 &
    df_src$merge_constituency == "School GIS", ]))
match_rate_gis_government <- as.numeric(round(
    (matched_gis_government / n_government) * 100, 1))

matched_gis_private <- as.numeric(nrow(df_src[df_src$private == 1 &
    df_src$merge_constituency == "School GIS", ]))
match_rate_gis_private <- as.numeric(round(
    (matched_gis_private / n_private) * 100, 1))

# Matched by Village Location
matched_village_government <- as.numeric(nrow(df_src[df_src$private == 0 &
    df_src$merge_constituency == "Village Locations", ]))
match_rate_village_government <- as.numeric(round(
    (matched_village_government /
        (n_government - matched_gis_government)) * 100, 1
))
overall_rate_village_government <- as.numeric(round(
    (matched_village_government / n_government) * 100, 1
))

matched_village_private <- as.numeric(nrow(df_src[df_src$private == 1 &
    df_src$merge_constituency == "Village Locations", ]))
match_rate_village_private <- as.numeric(round(
    (matched_village_private / (n_private - matched_village_private)) * 100, 1
))
overall_rate_village_private <- as.numeric(round(
    (matched_village_private / n_private) * 100, 1
))

# Matched by AAN:
matched_aan_government <- nrow(df_src[df_src$private == 0 &
    df_src$merge_constituency == "AAN", ])
match_rate_aan_government <- as.numeric(round(
    (matched_aan_government /
        (n_government - matched_gis_government - matched_village_government)) * 100, 1
))
overall_rate_aan_government <- as.numeric(round(
    (matched_aan_government / n_government) * 100, 1
))

matched_aan_private <- as.numeric(nrow(df_src[df_src$private == 1 &
    df_src$merge_constituency == "AAN", ]))
match_rate_aan_private <- as.numeric(round(
    (matched_aan_private / (n_private - matched_gis_private - matched_village_private)) *
    100, 1
))
overall_rate_aan_private <- as.numeric(round(
    (matched_aan_private / n_private) * 100, 1
))

# Matched by Pincodes:
matched_pincode_government <- as.numeric(nrow(df_src[df_src$private == 0 &
    df_src$merge_constituency == "Pincodes", ]))
match_rate_pincode_government <- as.numeric(round(
    (matched_pincode_government /
        (n_government - matched_village_government - matched_gis_government -
        matched_aan_government)) * 100, 1
))
overall_rate_pincode_government <- as.numeric(round(
    (matched_pincode_government / n_government) * 100, 1
))

matched_pincode_private <- as.numeric(nrow(df_src[df_src$private == 1 &
    df_src$merge_constituency == "Pincodes", ]))
match_rate_pincode_private <- as.numeric(round(
    (matched_pincode_private /
        (n_private - matched_village_private - matched_gis_private -
            matched_aan_private)) *
    100, 1
))
overall_rate_pincode_private <- as.numeric(round(
    (matched_pincode_private / n_private) * 100, 1
))

# Total Matches
total_government <- as.numeric(matched_gis_government +
    matched_village_government + matched_aan_government +
    matched_pincode_government)
total_private <- as.numeric(matched_gis_private + matched_village_private +
    matched_aan_private + matched_pincode_private)

match_rate_government <- round((total_government / n_government) * 100, 1)
match_rate_private <- round((total_private / n_private) * 100, 1)

# Overall match
sink(file = here("data/output/text/matchrate.tex"))
cat(round(((total_government + total_private) / (n_government + n_private)) *
    100, 1))
sink(file = NULL)

# Creating export table
gis_row <- c(n_government, matched_gis_government,
    match_rate_gis_government, match_rate_gis_government, n_private,
    matched_gis_private, match_rate_gis_private, match_rate_gis_private)
village_row <- c((n_government - matched_gis_government),
    matched_village_government, match_rate_village_government,
    overall_rate_village_government, (n_private - matched_gis_private),
    matched_village_private, match_rate_village_private,
    overall_rate_village_private)
aan_row <- c((n_government - matched_gis_government -
    matched_village_government), matched_aan_government,
    match_rate_aan_government, overall_rate_aan_government,
    (n_private - matched_gis_private - matched_village_private),
    matched_aan_private, match_rate_aan_private, overall_rate_aan_private)
pincode_row <- c(
    (n_government - matched_gis_government - matched_village_government -
        matched_aan_government), matched_pincode_government,
    match_rate_pincode_government, overall_rate_pincode_government,
    (n_private - matched_gis_private - matched_village_private -
        matched_aan_private), matched_pincode_private,
    match_rate_pincode_private, overall_rate_pincode_private)
total_row <- c(NA_integer_, total_government, NA_integer_,
    match_rate_government, NA_integer_, total_private, NA_integer_,
    match_rate_private)
row_names <- c("GIS", "Village Match", "AAN", "Pincodes", "Total")

match_table <- as.data.frame(rbind(gis_row, village_row, aan_row,
    pincode_row, total_row))
match_table <- cbind(row_names, match_table)

match_table_export <- xtable(match_table,
    align = "llrrrrrrrr",
    digits = c(0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0)
addtorow$command <- c(" & \\multicolumn{4}{c}{Government Schools} & \\multicolumn{4}{c}{Private Schools} \\\\\n",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9} \n",
    "Matching & Remaining & Number & Match & Overall Match & Remaining & Number & Match & Overall Match \\\\\n",
    "Strategy & Unmatched & Matched & Rate (\\%) & Rate (\\%) & Unmatched & Matched & Rate (\\%) & Rate (\\%) \\\\\n")

print(match_table_export,
    file = here("data/output/tables/matchratetable.tex"),
    include.colnames = FALSE,
    include.rownames = FALSE,
    booktabs = T,
    floating = F,
    add.to.row = addtorow,
    format.args = list(big.mark = ","),
    only.contents = F)