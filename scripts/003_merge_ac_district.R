###############################################################################
# Name: 002_merge_ac_district.R
# Purpose: Merges shapefiles of assembly constituencies with district level
# shapefiles
# Date created: 2020/09/18
###############################################################################

###############################################################################
# Loading packages
###############################################################################

library(sf)
library(dplyr)
library(stringr)
library(tibble)
library(tidylog)
library(janitor)
library(readstata13)
library(lubridate)
library(rio)
library(here)

# Loading shapefiles and data #################################################

# Loading Indian district shapefiles.  Downloaded from https://gadm.org/ on 2020/09/18
sf_district <- readRDS(here::here("data/raw/district maps/gadm36_IND_2_sf.rds"))

# Listing assembly constituency shapefiles
states <- list.files(path = here::here("data/raw/assembly constituency maps/"),
    pattern = ".shp", full.names = TRUE)

sf_use_s2(FALSE)

# Creating India wide shapefile
bindShapeFiles <- function(file) {

    df_shapefile <- st_read(file) %>%
        select(CONS_NAME, CONS_CODE, geometry)

    df_shapefile$centroid <- st_centroid(df_shapefile$geometry)

    return(df_shapefile)
}

sf_india <- lapply(states, bindShapeFiles) %>%
    bind_rows()

# Loading election data:
df_elections <- import(here("data/clean/coalition_parties.rds"))

# Loading School data:
load(here::here("data/clean/ihds_schools.RData"))

# Loading crosswalk data:
df_crosswalk <- read.dta13(here::here("data/raw/crosswalk/district_codes.dta"))

source("scripts/functions.R")

###############################################################################
# Cleaning data sources
###############################################################################

# Creating district shapefile
df_district <- sf_district %>%
    select(NAME_1, NAME_2) %>%
    rownames_to_column()

df_district <- as.data.frame(df_district)
df_district <- df_district %>%
    rename(row_name = rowname)

# Finding intersection of assembly constituency centroid and districts
within_centroids <- st_within(sf_india$centroid, sf_district)
within_centroids[sapply(within_centroids, function(x) length(x) == 0)] <- NA

intersection_vector <- unlist(within_centroids)

# Binding constituency to district shapefile
df_matched <- as.data.frame(cbind(sf_india, intersection_vector))
df_matched$intersection_vector <- as.character(df_matched$intersection_vector)

# Match shapefiles with districts data frame
# Nina wrote this code
df_matched$district <- df_district[intersection_vector,]$NAME_2
df_matched$state <- df_district[intersection_vector,]$NAME_1

df_matched_clean <- df_matched %>%
    select(CONS_CODE, CONS_NAME, district, state) %>%
    mutate(
        state_name = case_when(
            state == "Telangana" ~ "Andhra Pradesh",
            state == "Jammu and Kashmir" ~ "Jammu & Kashmir",
            state == "NCT of Delhi" ~ "Delhi",
            TRUE ~ state
        ),        
        weird_kachchh = ifelse(CONS_CODE == 0 & district == "Kachchh", 1, 0)
    ) %>%
    arrange(state_name, CONS_CODE) %>%
    rename(
        constituency_no = CONS_CODE,
        district_name = district
    ) %>%
    filter(!is.na(district_name)) %>%
    filter(weird_kachchh != 1) %>%
    select(-c(weird_kachchh, state)) %>%
    mutate(
        st_code2001 = case_when(
            state_name == "Andhra Pradesh" ~ 28,
            state_name == "Arunachal Pradesh" ~ 12,
            state_name == "Assam" ~ 18,
            state_name == "Bihar" ~ 10,
            state_name == "Chhattisgarh" ~ 22,
            state_name == "Delhi" ~ 7,
            state_name == "Goa" ~ 30,
            state_name == "Gujarat" ~ 24,
            state_name == "Haryana" ~ 6,
            state_name == "Himachal Pradesh" ~ 2,
            state_name == "Jammu & Kashmir" ~ 1,
            state_name == "Jharkhand" ~ 20,
            state_name == "Karnataka" ~ 29,
            state_name == "Kerala" ~ 32,
            state_name == "Madhya Pradesh" ~ 23,    
            state_name == "Maharashtra" ~ 27,
            state_name == "Manipur" ~ 14,
            state_name == "Meghalaya" ~ 17,
            state_name == "Mizoram" ~ 15,
            state_name == "Nagaland" ~ 13,
            state_name == "Odisha" ~ 21,
            state_name == "Puducherry" ~ 34,
            state_name == "Punjab" ~ 3,
            state_name == "Rajasthan" ~ 8,
            state_name == "Sikkim" ~ 11,
            state_name == "Tamil Nadu" ~ 33,
            state_name == "Telangana" ~ 28,
            state_name == "Tripura" ~ 16,
            state_name == "Uttar Pradesh" ~ 9,
            state_name == "Uttarakhand" ~ 5,
            state_name == "West Bengal" ~ 19,
            TRUE ~ NA_real_
        )
    ) %>%
    select(-state_name) %>%
    distinct()

# Cleaning election data:
df_elections_clean <- df_elections %>%
    select(
        st_code2001, state_name, year, constituency_no, constituency_name,
        month, by_election
    ) %>%
    ungroup() %>%
    arrange(state_name, constituency_name) %>%
    filter(year > 2007) %>%
    rename(
        month_election = month,
        year_election = year
    ) %>%
    distinct()

# Match constituency data with election results data
df_elections_districts <- left_join(df_elections_clean, df_matched_clean,
    by = c("st_code2001", "constituency_no"))

# Clean missing district names:

df_elections_districts <- df_elections_districts %>%
    mutate(
        state_name = str_to_lower(state_name),
        state_name = gsub(" ", "", state_name),
        district_name = str_to_lower(district_name),
        district_name = gsub(" ", "", district_name),

        # Cleaning state names
        state_name = ifelse(state_name == "nctofdelhi", "delhi", state_name),
        state_name = ifelse(state_name == "odisha", "orissa", state_name),
        state_name = ifelse(state_name == "puducherry", "pondicherry", state_name),
        state_name = ifelse(state_name == "uttarakhand", "uttaranchal", state_name),

        # Cleaning district names to match crosswalk district names:
        district_name = ifelse(district_name == "y.s.r." & state_name == "andhrapradesh", "cuddapah", district_name),
        
        district_name = ifelse(district_name == "longding" & state_name == "arunachalpradesh", "tirap", district_name),
        district_name = ifelse(district_name == "namsai" & state_name == "arunachalpradesh", "lohit", district_name),
        
        district_name = ifelse(district_name == "sivasagar" & state_name == "assam", "sibsagar", district_name),
        district_name = ifelse(district_name == "kamrupmetropolitan" & state_name == "assam", "kamrup(metro)", district_name),
        district_name = ifelse(district_name == "baksa" & state_name == "assam", "baska", district_name),
        district_name = ifelse(district_name == "dimahasao" & state_name == "assam", "northcacharhills", district_name),
        district_name = ifelse(district_name == "morigaon" & state_name == "assam", "marigaon", district_name),

        district_name = ifelse(district_name == "arwal" & state_name == "bihar", "arval", district_name),
        district_name = ifelse(district_name == "kaimur" & state_name == "bihar", "kaimur(bhabua)", district_name),

        district_name = ifelse(district_name == "uttarbastarkanker" & state_name == "chhattisgarh", "kanker", district_name),
        district_name = ifelse(district_name == "kabeerdham" & state_name == "chhattisgarh", "kawardha", district_name),

        district_name = ifelse(district_name == "aravalli" & state_name == "gujarat", "aravali", district_name),
        district_name = ifelse(district_name == "chhotaudaipur" & state_name == "gujarat", "Chhota Udaipur", district_name),
        district_name = ifelse(district_name == "dahod" & state_name == "gujarat", "dohad", district_name),
        district_name = ifelse(district_name == "devbhumidwarka" & state_name == "gujarat", "devbhoomidwarka", district_name),
        district_name = ifelse(district_name == "mahisagar" & state_name == "gujarat", "Mahisagar", district_name),

        district_name = ifelse(district_name == "bandipore" & state_name == "jammu&kashmir", "baramula", district_name),
        district_name = ifelse(district_name == "baramulla" & state_name == "jammu&kashmir", "baramula", district_name),
        district_name = ifelse(district_name == "ramban" & state_name == "jammu&kashmir", "doda", district_name),
        district_name = ifelse(district_name == "samba" & state_name == "jammu&kashmir", "jammu", district_name),
        district_name = ifelse(district_name == "rajouri" & state_name == "jammu&kashmir", "rajauri", district_name),
        district_name = ifelse(district_name == "kulgam" & state_name == "jammu&kashmir", "anantnag", district_name),
        district_name = ifelse(district_name == "reasi" & state_name == "jammu&kashmir", "udhampur", district_name),
        district_name = ifelse(district_name == "kulgam" & state_name == "jammu&kashmir", "anantnag", district_name),
        district_name = ifelse(district_name == "kishtwar" & state_name == "jammu&kashmir", "doda", district_name),
        district_name = ifelse(district_name == "ganderbal" & state_name == "jammu&kashmir", "srinagar", district_name),
        district_name = ifelse(district_name == "shupiyan" & state_name == "jammu&kashmir", "suphiyan", district_name),

        district_name = ifelse(district_name == "hazaribagh" & state_name == "jharkhand", "hazaribag", district_name),
        district_name = ifelse(district_name == "saraikela-kharsawan" & state_name == "jharkhand", "seraikelakharsw", district_name),
        district_name = ifelse(district_name == "jamtara" & state_name == "jharkhand", "jamta", district_name),
        district_name = ifelse(district_name == "khunti" & state_name == "jharkhand", "ranchi", district_name),
        district_name = ifelse(district_name == "simdega" & state_name == "jharkhand", "simde", district_name),
        district_name = ifelse(district_name == "latehar" & state_name == "jharkhand", "lateh", district_name),
        district_name = ifelse(district_name == "pakur" & state_name == "jharkhand", "pakaur", district_name),
        district_name = ifelse(district_name == "ramgarh" & state_name == "jharkhand", "hazaribag", district_name),

        district_name = ifelse(district_name == "chikballapura" & state_name == "karnataka", "chikkaballapur", district_name),
        district_name = ifelse(district_name == "chamrajnagar" & state_name == "karnataka", "chamarajanagar", district_name),
        district_name = ifelse(district_name == "ramanagara" & state_name == "karnataka", "ramnagara", district_name),
        district_name = ifelse(district_name == "yadgir" & state_name == "karnataka", "yadagiri", district_name),

        district_name = ifelse(district_name == "agarmalwa" & state_name == "madhyapradesh", "shajapur", district_name),
        district_name = ifelse(district_name == "alirajpur" & state_name == "madhyapradesh", "jhabua", district_name),
        district_name = ifelse(district_name == "anuppur" & state_name == "madhyapradesh", "anupp", district_name),
        district_name = ifelse(district_name == "ashoknagar" & state_name == "madhyapradesh", "ashoknag", district_name),
        district_name = ifelse(district_name == "burhanpur" & state_name == "madhyapradesh", "burhanp", district_name),
        district_name = ifelse(district_name == "singrauli" & state_name == "madhyapradesh", "sidhi", district_name),

        district_name = ifelse(district_name == "garhchiroli" & state_name == "maharashtra", "gadchiroli", district_name),
        district_name = ifelse(district_name == "mumbaisuburban" & state_name == "maharashtra", "mumbai suburban", district_name),
        district_name = ifelse(district_name == "mumbaicity" & state_name == "maharashtra", "mumbai", district_name),

        district_name = ifelse(district_name == "southwestgarohills" & state_name == "meghalaya", "westgarohills", district_name),
        district_name = ifelse(district_name == "northgarohills" & state_name == "meghalaya", "eastgarohills", district_name),
        district_name = ifelse(district_name == "southwestkhasihills" & state_name == "meghalaya", "westkhasihills", district_name),

        district_name = ifelse(district_name == "lawangtlai" & state_name == "mizoram", "lawngtlai", district_name),

        district_name = ifelse(district_name == "longleng" & state_name == "nagaland", "tuensang", district_name),
        district_name = ifelse(district_name == "peren" & state_name == "nagaland", "kohima", district_name),
        district_name = ifelse(district_name == "kiphire" & state_name == "nagaland", "tuensang", district_name),

        district_name = ifelse(district_name == "subarnapur" & state_name == "orissa", "sonapur", district_name),
        district_name = ifelse(district_name == "bauda" & state_name == "orissa", "baudh", district_name),

        district_name = ifelse(district_name == "puducherry" & state_name == "pondicherry", "pondicherry", district_name),

        district_name = ifelse(district_name == "fazilka" & state_name == "punjab", "firozpur", district_name),
        district_name = ifelse(district_name == "tarntaran" & state_name == "punjab", "tarn taran", district_name),
        district_name = ifelse(district_name == "shahidbhagatsinghnagar" & state_name == "punjab", "nawanshahr", district_name),
        district_name = ifelse(district_name == "barnala" & state_name == "punjab", "sangrur", district_name),
        district_name = ifelse(district_name == "pathankot" & state_name == "punjab", "gurdaspur", district_name),
        district_name = ifelse(district_name == "sahibzadaajitsinghnagar" & state_name == "punjab", "sas nagar", district_name),

        district_name = ifelse(district_name == "pratapgarh" & state_name == "rajasthan", "chittaurgarh", district_name),

        district_name = ifelse(district_name == "eastsikkim" & state_name == "sikkim", "east", district_name),
        district_name = ifelse(district_name == "southsikkim" & state_name == "sikkim", "south", district_name),
        district_name = ifelse(district_name == "westsikkim" & state_name == "sikkim", "west", district_name),
        district_name = ifelse(district_name == "northsikkim" & state_name == "sikkim", "north", district_name),

        district_name = ifelse(district_name == "virudunagar" & state_name == "tamilnadu", "virudhunagar", district_name),
        district_name = ifelse(district_name == "tiruppur" & state_name == "tamilnadu", "coimbatore", district_name),
        district_name = ifelse(district_name == "krishnagiri" & state_name == "tamilnadu", "kshnagiri", district_name),
        district_name = ifelse(district_name == "nagappattinam" & state_name == "tamilnadu", "nagapattinam", district_name),

        district_name = ifelse(district_name == "gomati" & state_name == "tripura", "southtripura", district_name),
        district_name = ifelse(district_name == "khowai" & state_name == "tripura", "westtripura", district_name),
        district_name = ifelse(district_name == "sipahijala" & state_name == "tripura", "westtripura", district_name),
        district_name = ifelse(district_name == "unokoti" & state_name == "tripura", "northtripura", district_name),

        district_name = ifelse(district_name == "kasganj" & state_name == "uttarpradesh", "etah", district_name),
        district_name = ifelse(district_name == "amethi" & state_name == "uttarpradesh", "chatripoatisahujimahrajnagar", district_name),
        district_name = ifelse(district_name == "amroha" & state_name == "uttarpradesh", "moradabad", district_name),
        district_name = ifelse(district_name == "sambhal" & state_name == "uttarpradesh", "moradabad", district_name),
        district_name = ifelse(district_name == "santravidasnagar" & state_name == "uttarpradesh", "santravidasnagarbhadohi", district_name),
        district_name = ifelse(district_name == "shravasti" & state_name == "uttarpradesh", "shrawasti", district_name),
        district_name = ifelse(district_name == "lakhimpurkheri" & state_name == "uttarpradesh", "kheri", district_name),
        district_name = ifelse(district_name == "hapur" & state_name == "uttarpradesh", "ghaziabad", district_name),
        district_name = ifelse(district_name == "shamli" & state_name == "uttarpradesh", "muzaffarnagar", district_name),
        district_name = ifelse(is.na(district_name) & state_name == "uttarpradesh" & constituency_name == "Kheragarh", "agra", district_name),

        district_name = ifelse(district_name == "alipurduar" & state_name == "westbengal", "jalpaiguri", district_name),
        district_name = ifelse(district_name == "north24parganas" & state_name == "westbengal", "northtwentyfourparganas", district_name),
        district_name = ifelse(district_name == "south24parganas" & state_name == "westbengal", "southtwentyfourparganas", district_name),
        district_name = ifelse(district_name == "pashchimmedinipur" & state_name == "westbengal", "paschimmidnapor", district_name),
    )

# Cleaning crosswalk and merging with constituencies
df_crosswalk_clean <- df_crosswalk %>%
    select(state_name, district_name, district_code, state_district) %>%
    filter(
        state_name != "all-india",
        district_code != 0
    ) %>%
    distinct(state_name, district_name, district_code, .keep_all = TRUE)

# Merging with elections data:
df_matched_elections_districts <- left_join(df_elections_districts,
    df_crosswalk_clean, by = c("state_name", "district_name")) %>%
    mutate(
        district_code = ifelse(state_name == "delhi", 1, district_code),
        district_code = ifelse(state_name == "uttarpradesh" & constituency_name == "Amroha", 6, district_code),
        district_code = ifelse(state_name == "uttarpradesh" & constituency_name == "Dhanaura", 6, district_code),
        district_code = ifelse(state_name == "uttarpradesh" & constituency_name == "Hasanpur", 6, district_code),
        district_code = ifelse(state_name == "uttarpradesh" & constituency_name == "Garhmukteshwar", 6, district_code),
        district_code = ifelse(state_name == "uttarpradesh" & constituency_name == "Naugawan Sadat", 6, district_code)
    ) %>%
    select(-state_district)

# Create district-level election variables ####################################

df_matched_elections_districts <- df_matched_elections_districts %>%
    group_by(state_name, district_name, year_election) %>%
    mutate(number_constituencies = n())

# Cleaning IHDS Data School Level Data:
df_ihds_clean <- distinct(df_ihds_school_clean) %>%
    mutate(district_code = ifelse(state_name == "delhi", 1, district_code))
    # Creating fake Delhi district codes to allow for matching

# Calculate share of constituencies within a district that hold elections #####

df_constituency_in_district <- df_elections_districts %>%
    filter(
        !is.na(state_name), !is.na(constituency_name), !is.na(district_name)
    ) %>%
    distinct(state_name, district_name, constituency_name) %>%
    count(state_name, district_name) %>%
    rename(n_constituency_in_district = n)

df_constituency_district_share <- df_elections_districts %>%
    count(state_name, district_name, year_election) %>%
    rename(n_elections_district_year = n) %>%
    left_join(
        ., df_constituency_in_district, by = c("state_name", "district_name")
    ) %>%
    mutate(
        share_constituencies_election = n_elections_district_year / n_constituency_in_district,
        share_constituencies_election = ifelse(
            share_constituencies_election > 1,
            1,
            share_constituencies_election
        )
    ) %>%
    filter(!is.na(share_constituencies_election))

df_matched_elections_districts <- left_join(
    df_matched_elections_districts,
    df_constituency_district_share,
    by = c("state_name", "district_name", "year_election")
)

# Manually adding election dates for seven sisters ############################
# Have to add this as in IHDS they don't have district information

df_matched_elections_districts <- df_matched_elections_districts %>%
    ungroup() %>%
    add_row(
        state_name = "sikkim", district_code = 0, year_election = 2014, month_election = 4, by_election = 0
    ) %>%
    add_row(
        state_name = "sikkim", district_code = 0, year_election = 2009, month_election = 4, by_election = 0
    ) %>%
    add_row(
        state_name = "arunachalpradesh", district_code = 0, year_election = 2014, month_election = 4, by_election = 0
    ) %>%
    add_row(
        state_name = "arunachalpradesh", district_code = 0, year_election = 2009, month_election = 10, by_election = 0
    ) %>%
    add_row(
        state_name = "nagaland", district_code = 0, year_election = 2018,month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "nagaland", district_code = 0, year_election = 2013, month_election = 3, by_election = 0
    ) %>%
    add_row(
        state_name = "nagaland", district_code = 0, year_election = 2008, month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "manipur", district_code = 0, year_election = 2017, month_election = 3, by_election = 0
    ) %>%
    add_row(
        state_name = "manipur", district_code = 0, year_election = 2012, month_election = 1, by_election = 0
    ) %>%
    add_row(
        state_name = "manipur", district_code = 0, year_election = 2007, month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "mizoram", district_code = 0, year_election = 2018, month_election = 11, by_election = 0
    ) %>%
    add_row(
        state_name = "mizoram", district_code = 0, year_election = 2013, month_election = 11, by_election = 0
    ) %>%
    add_row(
        state_name = "mizoram", district_code = 0, year_election = 2008, month_election = 12, by_election = 0
    ) %>%
    add_row(
        state_name = "tripura", district_code = 0, year_election = 2018, month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "tripura", district_code = 0, year_election = 2013, month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "tripura", district_code = 0, year_election = 2008, month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "meghalaya", district_code = 0, year_election = 2018, month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "meghalaya", district_code = 0, year_election = 2013, month_election = 2, by_election = 0
    ) %>%
    add_row(
        state_name = "meghalaya", district_code = 0, year_election = 2008, month_election = 3, by_election = 0
    ) %>%
    add_row(
        state_name = "pondicherry", district_code = 0, year_election = 2016, month_election = 5, by_election = 0
    ) %>%
    add_row(
        state_name = "pondicherry", district_code = 0, year_election = 2011, month_election = 4, by_election = 0
    )

# Matching IHDS with matched electoral constituency and districts #############

# Matching IHDS with districts
df_ihds_elections_match <- df_ihds_clean %>%
    select(-c(state)) %>%
    left_join(., df_matched_elections_districts,
        by = c("state_name", "district_code")
    )

# Manually clean unmatched districts ##########################################
# There are 9 districts with no matches.  Cleaning districts that do not have a
# match manually

df_elections_ihds_no_match <- df_matched_elections_districts %>%
    anti_join(
        ., df_ihds_clean, by = c("state_name", "district_code")
    ) %>%
    ungroup() %>%
    select(state_name, district_name, district_code) %>%
    arrange(state_name, district_code, district_name) %>%
    distinct()

df_ihds_elections_no_match <- df_ihds_clean %>%
    anti_join(
        ., df_matched_elections_districts, by = c("state_name", "district_code")
    ) %>%
    ungroup() %>%
    select(state_name, district_code) %>%
    arrange(state_name, district_code) %>%
    distinct()

export(
    df_elections_ihds_no_match,
    file.path(here(), "data/temporary/missing_ihds.xlsx")
)

export(
    df_ihds_elections_no_match,
    file.path(here(), "data/temporary/missing_elections.xlsx")
)

df_districts_unmatched <- df_crosswalk_clean %>%
    right_join(
        ., df_ihds_elections_no_match, by = c("state_name", "district_code")
    )

df_elections_districts %>%
    filter(state_name == "uttarpradesh") %>%
    tabyl(district_name)
df_elections_districts %>%
    filter(state_name == "andhrapradesh") %>%
    tabyl(district_name)

df_ihds_elections_match_clean <- df_ihds_elections_match %>%
    mutate(
        day_election = 1,
        date_election_all = dmy(
            paste(day_election, month_election, year_election,
            sep = "/")
        ),
        date_election = dmy(
            paste(day_election, month_election, year_election,
            sep = "/")
        ),
        months_to_election_all = interval(date_survey, date_election_all),
        months_to_election_all = months_to_election_all %/% months(1),
        absolute_time_all = abs(months_to_election_all)
    )

df_ihds_elections_match_clean_no_by <- df_ihds_elections_match %>%
    filter(by_election == 0) %>%
    mutate(
        day_election = 1,
        date_election_no_by = dmy(
            paste(day_election, month_election, year_election,
            sep = "/")
        ),
        months_to_election_no_by = interval(date_survey, date_election_no_by),
        months_to_election_no_by = months_to_election_no_by %/% months(1),
        absolute_time_no_by = abs(months_to_election_no_by)
    )

# Creating dataframe that gives us which districts are unmatched with electoral
# constituencies. Will give us the number of electoral constituencies that have
# NA as their district code:
df_unmatched_districts <- df_matched_elections_districts %>%
    group_by(state_name, district_code) %>%
    tally()

# Creating dataframe that gives us the average month for the specific states 
# that we have trouble matching
election_states_problems <- df_matched_elections_districts %>%
    filter(
        state_name == "delhi" | state_name == "andhrapradesh" |
        state_name == "uttarpradesh"
    ) %>% 
    group_by(state_name, district_code) %>% 
    summarise(month = mean(month_election))

# Gives us the specific constituencies that we have problems with:
election_states_raw <- df_matched_elections_districts %>%
    filter(
        state_name == "delhi" | state_name == "andhrapradesh" |
        state_name == "uttarpradesh"
    )

# Gives us the districts in the IHDS data that _should_ be matched
ihds_districts <- df_ihds_clean %>%
    group_by(state_name, district_code) %>%
    tally()

# Gives us the electoral constituencies that have missing election dates. This
# could either be a lubridate problem, or a problem with missing dates:
bad_dates <- df_ihds_elections_match_clean %>%
    filter(is.na(date_election)) %>%
    select(
        day_election, month_election, year_election, date_election,
        everything()
    )

# Gives us the _states_ that have bad election dates:
bad_states <- df_ihds_elections_match_clean %>%
    group_by(state_name, year_election) %>%
    tally()

# Calculating which is the closest election including by-elections ############

df_closest_election_all <- df_ihds_elections_match_clean %>%
    filter(!is.na(absolute_time_all)) %>%
    group_by(school_id) %>%
    summarise(min_absolute_time_all = min(absolute_time_all, na.rm = TRUE))

df_ihds_single_election_all <- left_join(df_closest_election_all,
    df_ihds_elections_match_clean, by = c("school_id")
)

# Calculating which is the closest election excluding by-elections ############

df_closest_election_no_by <- df_ihds_elections_match_clean_no_by %>%
    filter(!is.na(absolute_time_no_by)) %>%
    group_by(school_id) %>%
    summarise(min_absolute_time_no_by = min(absolute_time_no_by, na.rm = TRUE))

df_ihds_single_election_no_by <- left_join(df_closest_election_no_by,
    df_ihds_elections_match_clean_no_by, by = c("school_id")
)

# Calculating time to all elections including by-elections ####################

df_ihds_single_election_all <- df_ihds_single_election_all %>%
    mutate(zero = absolute_time_all - min_absolute_time_all) %>%
    filter(zero == 0) %>%
    select(
        id_psu, school_id, months_to_election_all, absolute_time_all,
        teachers, private, students, date_survey, state_name,
        starts_with("by_e"), share_constituencies_election, state_district,
        district_name
    ) %>%
    mutate(
        years_to_election_all = case_when(
            months_to_election_all >= 0 & months_to_election_all < 12 ~ 0,
            months_to_election_all >= 12 & months_to_election_all < 24 ~ -1,
            months_to_election_all >= 24 ~ -2,
            months_to_election_all < 0 & months_to_election_all >= -12 ~ 1,
            months_to_election_all < -12 ~ 2,
            TRUE ~ NA_real_
        ),
        years_to_election_all = as.factor(years_to_election_all),
        election_year_all = ifelse(years_to_election_all == 0, 1, 0)
    ) %>%
    distinct(.keep_all = TRUE)

# Calculating time to elections excluding by-elections ########################

df_ihds_single_election_no_by <- df_ihds_single_election_no_by %>%
    mutate(zero = absolute_time_no_by - min_absolute_time_no_by) %>%
    filter(zero == 0) %>%
    select(school_id, months_to_election_no_by) %>%
    mutate(
        years_to_election_no_by = case_when(
            months_to_election_no_by >= 0 & months_to_election_no_by < 12 ~ 0,
            months_to_election_no_by >= 12 & months_to_election_no_by < 24 ~ -1,
            months_to_election_no_by >= 24 ~ -2,
            months_to_election_no_by < 0 & months_to_election_no_by >= -12 ~ 1,
            months_to_election_no_by < -12 ~ 2,
            TRUE ~ NA_real_
        ),
        years_to_election_no_by = as.factor(years_to_election_no_by),
        election_year_no_by = ifelse(years_to_election_no_by == 0, 1, 0)
    ) %>%
    distinct(.keep_all = TRUE)

# Joining two dataframes with election timing #################################

df_ihds_school_clean <- left_join(
    df_ihds_single_election_all,
    df_ihds_single_election_no_by,
    by = "school_id"
)

# Clean schools that are equidistant between two elections ####################
# Also cleans schools that because of the way I am setting the election date
# end up being in the wrong election year

df_ihds_school_clean <- df_ihds_school_clean %>%
    group_by(school_id) %>%
    mutate(id = row_number()) %>%
    ungroup() %>%
    mutate(
        drop = case_when(
            school_id == 202011 & by_election == 1 ~ 1,
            school_id == 202012 & by_election == 1 ~ 1,
            school_id == 202031 & by_election == 1 ~ 1,
            school_id == 202032 & by_election == 1 ~ 1,
            school_id == 2002071 & id == 1 ~ 1,
            school_id == 2002072 & id == 1 ~ 1,
            school_id == 2014141 & id == 1 ~ 1,
            TRUE ~ 0
        )
    ) %>%
    clean_ihds_election_years(.) %>%
    mutate(years_to_election_no_by = as.factor(years_to_election_no_by)) %>%
    filter(drop != 1) %>%
    select(-drop, -id)

df_ihds_school_clean <- within(df_ihds_school_clean, years_to_election_all <- relevel(years_to_election_all, ref = "0"))
df_ihds_school_clean <- within(df_ihds_school_clean, years_to_election_no_by <- relevel(years_to_election_no_by, ref = "0"))

# Saving clean data ###########################################################

save(df_ihds_school_clean, file = here::here("data/clean/ihds_schools_elections.RData"))
