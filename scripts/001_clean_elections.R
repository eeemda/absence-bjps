# Metadata ####################################################################
# Name: 004_clean_elections.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Clean elections

# Loading packages ############################################################

library(data.table)
library(rio)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readstata13)
library(here)

# Sourcing functions ##########################################################

source(here("scripts/functions.R"))

# Cleaning election data ######################################################
# The main source for cleaning the month of the by-election was the ECI's
# data on key statistics for the by-elections from the wayback machine
# https://web.archive.org/web/20210416210953/https://eci.gov.in/ByeElection/ByeMay2000/index.htm
# The ECI's main site was down during the elections in 2024.
# Wikipedia's by-election and yearly elections site was also a reliable source

# Loading election data from Lok Dabha
df_elections_raw <- import(here("data/raw/elections/ac_elections.rda"))

df_elections <- df_elections_raw %>%
    clean_elections(.) %>%
    distinct(
        state_name, assembly_no, constituency_no, year, poll_no, candidate,
        .keep_all =TRUE
    ) %>%
    filter(position == 1) %>%
    select(
        state_name, st_code_ec, st_code2001, constituency_no, assembly_no,
        poll_no, constituency_name, year, month, by_election, valid_votes,
        electors, margin, enop, party
    ) %>%
    mutate(
        election_year = 1,
        margin = (margin / valid_votes) * 100,
        turnout = valid_votes / electors,
        constituency_code_post = as.numeric(ifelse(year >= 2008,
            constituency_no, NA)),
        constituency_code_pre = as.numeric(ifelse(year <= 2007,
            constituency_no, NA)),
        unique_id_post = ifelse(year >= 2008, paste0(st_code_ec,
            constituency_no, year), NA),
        unique_id_pre = ifelse(year <= 2007,  paste0(st_code_ec,
            constituency_no, year), NA),
        unique_id = paste0(state_name, year, party),
        by_election = ifelse(by_election == 1, 1, 0),
        merge = "master"
    ) %>%
    select(-c(valid_votes, electors)) %>%
    distinct(unique_id_post, unique_id_pre, .keep_all = TRUE) %>%
    arrange(st_code2001, constituency_name, year) %>%
    group_by(st_code2001, constituency_name) %>%
    mutate(election_number = row_number()) %>%
    ungroup() %>%
    arrange(st_code2001, constituency_name, election_number) %>%
    group_by(st_code2001, constituency_name) %>%
    mutate(
        by_election_previous = lead(by_election),
        by_election_next = lag(by_election),
        across(starts_with("by_election_"), ~ ifelse(is.na(.), 0, .))
    ) %>%
    ungroup()

# Creating consistent constituency number #####################################

df_elections <- df_elections %>%
    group_by(st_code2001, constituency_name) %>%
    fill(constituency_code_pre, .direction = "down") %>%
    fill(constituency_code_post, .direction = "up") %>%
    ungroup() %>%
    mutate(
        constituency_code_ap = case_when(
            st_code2001 == 28 & year > 2009 ~ constituency_code_post,
            TRUE ~ NA_real_
        )
    ) %>%
    group_by(st_code2001, constituency_name) %>%
    fill(constituency_code_ap, .direction = "up") %>%
    ungroup() %>%
    mutate(
        constituency_no = case_when(
            st_code2001 != 28 ~ constituency_code_post,
            st_code2001 == 28 & year <= 2009 ~ constituency_code_ap,
            st_code2001 == 28 & year > 2009 ~ constituency_code_post
        )
    )

# Loading alignment data ######################################################

df_alignment <- import(here("data/raw/alignment/coalitions_parties.dta")) %>%
    select(pc01_state_name, year, party, coalition) %>%
    rename(state_name = pc01_state_name) %>%
    mutate(
        year = as.integer(year),
        state_name = str_to_title(state_name),
        state_name = ifelse(state_name == "Orissa", "Odisha", state_name),
        state_name = ifelse(state_name == "Uttaranchal", "Uttarakhand",
            state_name),
        state_name = ifelse(state_name == "Pondicherry", "Puducherry",
            state_name),

        merge = "using",
        unique_id = paste0(state_name, year, party),

        party = ifelse(party == "AIADMK", "ADMK", party),
        party = ifelse(party == "All India Majlis-E-Ittehadul Muslimeen",
            "AIMIM", party),
        party = ifelse(party == "CPM" & state_name == "Tripura" & year == 2013,
            "CPI(M)", party),
        party = ifelse(party == "Navodyam Party", "NPT", party),
        party = ifelse(party == "Samata Kranti Dal", "SKD", party),
        party = ifelse(party == "Yuvajana Sramika Rythu Congress Party",
            "YSRCP", party),
    )

###############################################################################
# Merging electoral data with alignment data
###############################################################################

df_elections <- left_join(df_elections, df_alignment,
    by = c("state_name", "year", "party"))

df_elections$merge <- apply(df_elections[c("merge.x", "merge.y")], 1,
    function(x) paste(na.omit(x), collapse = ""))

df_elections <- df_elections %>%
    mutate(unique_id = ifelse(unique_id.x %in% NA, unique_id.y, unique_id.x)) %>%
    select(-merge.x, -merge.y, -unique_id.x, -unique_id.y)

df_elections <- df_elections %>%
    mutate(
        # Goa 2012:
        coalition = ifelse((party == "BJP" | party == "MGP") & year == 2012 &
            state_name == "Goa", 1, coalition),
        coalition = ifelse((party != "BJP" & party != "MGP") & year == 2012 &
            state_name == "Goa", 0, coalition),

        # Manipur 2012:
        coalition = ifelse(party == "INC" & year == 2012 &
            state_name == "Manipur", 1, coalition),
        coalition = ifelse(party != "INC" & year == 2012 &
            state_name == "Manipur", 0, coalition),

        # Delhi 2013:
        coalition = ifelse((party == "AAP" | party == "INC") &
            state_name == "Delhi" & year == 2013, 1, coalition),
        coalition = ifelse(party != "AAP" & party != "INC" &
            state_name == "Delhi" & year == 2013, 0, coalition),

        # Meghalaya 2013:
        coalition = ifelse((party == "INC" | party == "UDP") &
            state_name == "Meghalaya" & year == 2013, 1, coalition),
        coalition = ifelse((party != "INC" | party != "UDP") &
            state_name == "Meghalaya" & year == 2013, 0, coalition),
        
        # Mizoram 2013:
        coalition = ifelse(party == "INC" & state_name == "Mizoram" &
            year == 2013, 1, coalition),
        coalition = ifelse(party != "INC" & state_name == "Mizoram" &
            year == 2013, 0, coalition),

        # Nagaland 2013:
        # Sources:
        # https://news.biharprabha.com/2013/02/nagaland-peoples-front-all-set-to-win-2013-assembly-polls/
        # https://timesofindia.indiatimes.com/city/guwahati/3-NCP-MLAs-in-Nagaland-join-BJP/articleshow/36756643.cms
        # https://en.wikipedia.org/wiki/2013_Nagaland_Legislative_Assembly_election
        coalition = ifelse((party == "NPF" | party == "BJP" | party == "NCP" |
            party == "JD(U)") & state_name == "Nagaland" & year == 2013, 1,
            coalition),
        coalition = ifelse(party != "NPF" & party != "BJP" & party != "NCP" &
            party != "JD(U)" & state_name == "Nagaland" & year == 2013, 0,
            coalition),

        # Arunachal Pradesh 2014:
        # Sources:
        # https://en.wikipedia.org/wiki/2014_Arunachal_Pradesh_Legislative_Assembly_election
        coalition = ifelse(party == "INC" & state_name == "Arunachal Pradesh"
            & year == 2014, 1, coalition),
        coalition = ifelse(party != "INC" & state_name == "Arunachal Pradesh"
            & year == 2014, 1, coalition),

        # Haryana 2014:
        # Sources:
        # https://www.ndtv.com/assembly/alliance-with-inld-not-to-sour-ties-with-bjp-akali-dal-leader-sukhbir-singh-badal-676114
        coalition = ifelse(party == "BJP" & state_name == "Haryana"
            & year == 2014, 1, coalition),
        coalition = ifelse(party != "BJP" & state_name == "Haryana"
            & year == 2014, 0, coalition),

        # Jammu & Kashmir 2014:
        # Sources:
        # https://en.wikipedia.org/wiki/2014_Jammu_and_Kashmir_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "JKPDP") & year == 2014 &
            state_name == "Jammu & Kashmir", 1, coalition),
        coalition = ifelse((party != "BJP" & party != "JKPDP") & year == 2014 &
            state_name == "Jammu & Kashmir", 1, coalition),

        # Jharkhand 2014:
        # Sources:
        # https://en.wikipedia.org/wiki/2014_Jharkhand_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "AJSUP" | party == "JVM")
            & state_name == "Jharkhand" & year == 2014, 1, coalition),
        coalition = ifelse((party != "BJP" & party != "AJSUP" & party != "JVM")
            & state_name == "Jharkhand" & year == 2014, 0, coalition),

        # Maharashtra 2014:
        # Sources
        # https://en.wikipedia.org/wiki/2014_Maharashtra_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "SHS") &
            state_name == "Maharashtra" & year == 2014, 1, coalition),
        coalition = ifelse(party != "BJP" & party != "SHS" &
            state_name == "Maharashtra" & year == 2014, 0, coalition),

        # Sikkim 2014:
        coalition = ifelse(party == "SDF" & state_name == "Sikkim"
            & year == 2014, 1, coalition),
        coalition = ifelse(party == "SKM" & state_name == "Sikkim"
            & year == 2014, 0, coalition),

        # Telangana 2014:
        coalition = ifelse(party == "TRS" & state_name == "Telangana"
            & year == 2014, 1, coalition),
        coalition = ifelse(party != "TRS" & state_name == "Telangana"
            & year == 2014, 0, coalition),

        # Bihar 2015:
        # Sources:
        # https://en.wikipedia.org/wiki/2015_Bihar_Legislative_Assembly_election#Mahagathbandhan
        coalition = ifelse((party == "JD(U)" | party == "BJP") &
            state_name == "Bihar" & year == 2015, 1, coalition),
        coalition = ifelse(party != "JD(U)" & party != "BJP" &
            state_name == "Bihar" & year == 2015, 0, coalition),

        # Delhi 2015
        coalition = ifelse(party == "AAP" & state_name == "Delhi" &
            year == 2015, 1, coalition),
        coalition = ifelse(party != "AAP" & state_name == "Delhi" &
            year == 2015, 0, coalition),

        # Assam 2016:
        # Sources:
        # https://en.wikipedia.org/wiki/2016_Assam_Legislative_Assembly_election#Share
        coalition = ifelse((party == "BJP" | party == "AGP" | party == "BOPF")
            & state_name == "Assam" & year == 2016, 1, coalition),
        coalition = ifelse(party != "BJP" & party != "AGP" & party != "BOPF"
            & state_name == "Assam" & year == 2016, 0, coalition),

        # Kerala 2016
        # Sources:
        # https://en.wikipedia.org/wiki/2016_Kerala_Legislative_Assembly_election#Left_Democratic_Front
        coalition = ifelse((party == "CPI(M)" | party == "CPI" |
            party == "JD(S)" | party == "NCP" | party == "C(S)") &
            year == 2016 & state_name == "Kerala", 1, coalition),

        # Puducherry 2016:
        # Sources:
        # https://en.wikipedia.org/wiki/2016_Puducherry_Legislative_Assembly_election
        coalition = ifelse((party == "INC" | party == "DMK") & year == 2016 &
            state_name == "Puducherry", 1, coalition),
        coalition = ifelse(party != "INC" & party != "DMK" & year == 2016 &
            state_name == "Puducherry", 0, coalition),

        # Tamil Nadu 2016:
        # Sources:
        # https://en.wikipedia.org/wiki/2016_Tamil_Nadu_Legislative_Assembly_election
        coalition = ifelse(party == "ADMK" & year == 2016 &
            state_name == "Tamil Nadu", 1, coalition),
        coalition = ifelse(party != "ADMK" & year == 2016 &
            state_name == "Tamil Nadu", 0, coalition),

        # Telangana by-elections 2016
        coalition = ifelse(party == "TRS" & year == 2016 &
            state_name == "Telangana", 1, coalition),
        coalition = ifelse(party != "TRS" & year == 2016 &
            state_name == "Telangana", 0, coalition),

        # West Bengal 2016:
        coalition = ifelse(party == "AITC" & year == 2016 &
            state_name == "West Bengal", 1, coalition),
        coalition = ifelse(party != "AITC" & year == 2016 &
            state_name == "West Bengal", 0, coalition),

        # Goa 2017:
        # Sources:
        # https://en.wikipedia.org/wiki/2017_Goa_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "MG" | party == "GFP")
            & year == 2017 & state_name == "Goa", 1, coalition),
        coalition = ifelse(party != "BJP" & party != "MG" & party != "GFP"
            & year == 2017 & state_name == "Goa", 0, coalition),

        # Gujarat 2017:
        # Sources:
        # https://en.wikipedia.org/wiki/2022_Gujarat_Legislative_Assembly_election
        coalition = ifelse(party == "BJP" & year == 2017 &
            state_name == "Gujarat", 1, coalition),
        coalition = ifelse(party != "BJP" & year == 2017 &
            state_name == "Gujarat", 0, coalition),

        # Himachal Pradesh 2017:
        # Sources:
        # https://en.wikipedia.org/wiki/2017_Himachal_Pradesh_Legislative_Assembly_election
        coalition = ifelse(party == "BJP" & year == 2017 &
            state_name == "Himachal Pradesh", 1, coalition),
        coalition = ifelse(party != "BJP" & year == 2017 &
            state_name == "Himachal Pradesh", 0, coalition),

        # Manipur 2017:
        # Sources:
        # https://en.wikipedia.org/wiki/2022_Manipur_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "NPP" | party == "NPF" |
            party == "LJP") & year == 2017 & state_name == "Manipur", 1,
            coalition),
        coalition = ifelse(party != "BJP" & party != "NPP" & party != "NPF" &
            party != "LJP" & year == 2017 & state_name == "Manipur", 0,
            coalition),

        # Punjab 2017:
        # Sources:
        # https://en.wikipedia.org/wiki/2017_Punjab_Legislative_Assembly_election
        coalition = ifelse(party == "INC" & state_name == "Punjab" &
            year == 2017, 1, coalition),
        coalition = ifelse(party != "INC" & state_name == "Punjab" &
            year == 2017, 0, coalition),

        # Uttarakhand 2017:
        # Source:
        # https://en.wikipedia.org/wiki/2022_Uttarakhand_Legislative_Assembly_election
        coalition = ifelse(party == "BJP" & state_name == "Uttarakhand" &
            year == 2017, 1, coalition),
        coalition = ifelse(party != "BJP" & state_name == "Uttarakhand" &
            year == 2017, 0, coalition),

        # Uttar Pradesh 2017:
        # Source:
        # https://en.wikipedia.org/wiki/2017_Uttar_Pradesh_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "ADS") & year == 2017 &
            state_name == "Uttar Pradesh", 1, coalition),
        coalition = ifelse(party != "BJP" & party != "ADS" & year == 2017 &
            state_name == "Uttar Pradesh", 0, coalition),

        # Chhattisgarh 2018:
        # Source:
        # https://en.wikipedia.org/wiki/2018_Chhattisgarh_Legislative_Assembly_election
        coalition = ifelse(party == "INC" & year == 2018 &
            state_name == "Chhattisgarh", 1, coalition),
        coalition = ifelse(party != "INC" & year == 2018 &
            state_name == "Chhattisgarh", 0, coalition),

        # Karnataka 2018:
        # Source:
        # https://en.wikipedia.org/wiki/2018_Karnataka_Legislative_Assembly_election#cite_note-61
        coalition = ifelse((party == "INC" | party == "JD(S)" | party == "BSP")
            & year == 2018 & state_name == "Karnataka", 1, coalition),
        coalition = ifelse(party != "INC" & party != "JD(S)" & party != "BSP"
            & year == 2018 & state_name == "Karnataka", 0, coalition),

        # Madhya Pradesh 2018:
        # Source:
        # https://en.wikipedia.org/wiki/2018_Madhya_Pradesh_Legislative_Assembly_election
        # https://timesofindia.indiatimes.com/india/mp-cliffhanger-ends-congress-single-largest-party-with-114-seats-bjp-gets-109/articleshow/67054167.cms
        # https://en.wikipedia.org/wiki/2020_Madhya_Pradesh_political_crisis
        coalition = ifelse((party == "INC" | party == "IND" | party == "SP") &
            year == 2018 & state_name == "Madhya Pradesh", 1, coalition),
        coalition = ifelse(party == "BJP" & year == 2018 &
            state_name == "Madhya Pradesh", 0, coalition),

        # Meghalaya 2018:
        # Source
        # https://en.wikipedia.org/wiki/Conrad_Sangma
        coalition = ifelse((party == "NPP" | party == "UDP" | party == "PDF" |
            party == "HSPDP" | party == "BJP") & state_name == "Meghalaya" &
            year == 2018, 1, coalition),
        coalition = ifelse(party == "INC" & state_name == "Meghalaya" &
            year == 2018, 0, coalition),

        # Mizoram 2018:
        # Source: https://www.ndtv.com/india-news/mizoram-election-results-2018-early-christmas-for-mnf-zoramthanga-to-be-new-chief-minister-1961230
        coalition = ifelse(party == "MNF" & state_name == "Mizoram" &
            year == 2018, 1, coalition),
        coalition = ifelse(party == "INC" & state_name == "Mizoram" &
            year == 2018, 0, coalition),

        # Nagaland 2018:
        # Source:
        # https://en.wikipedia.org/wiki/2018_Nagaland_Legislative_Assembly_election
        # https://www.indiatoday.in/pti-feed/story/senior-politician-neiphiu-rio-back-as-nagaland-cm-1185069-2018-03-08
        coalition = ifelse((party == "NDPP" | party == "BJP" | party == "IND")
            & state_name == "Nagaland" & year == 2018, 1, coalition),
        coalition = ifelse(party != "NDPP" & party != "BJP" & party != "IND"
            & state_name == "Nagaland" & year == 2018, 0, coalition),

        # Rajasthan 2018:
        # Source:
        # https://en.wikipedia.org/wiki/2018_Rajasthan_Legislative_Assembly_election
        coalition = ifelse(party == "INC" & state_name == "Rajasthan" &
            year == 2018, 1, coalition),
        coalition = ifelse(party != "INC" & state_name == "Rajasthan" &
            year == 2018, 0, coalition),

        # Telangana 2018:
        # Source
        # https://en.wikipedia.org/wiki/2018_Telangana_Legislative_Assembly_election
        coalition = ifelse(party == "TRS" & state_name == "Telangana" &
            year == 2018, 1, coalition),
        coalition = ifelse(party != "TRS" & state_name == "Telangana" &
            year == 2018, 0, coalition),

        # Tripura 2018:
        # Source
        # https://en.wikipedia.org/wiki/2018_Tripura_Legislative_Assembly_election
        coalition = ifelse(party == "BJP" & state_name == "Tripura" &
            year == 2018, 1, coalition),
        coalition = ifelse(party != "BJP" & state_name == "Tripura" &
            year == 2018, 0, coalition),

        # Arunachal Pradesh 2019:
        # Source
        # https://en.wikipedia.org/wiki/2019_Arunachal_Pradesh_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "JD(U)" |
            party == "NPP") & state_name == "Arunachal Pradesh" & year == 2019,
            1, coalition),
        coalition = ifelse(party != "BJP" & party != "JD(U)" &
            party != "NPP" & state_name == "Arunachal Pradesh" & year == 2019,
            0, coalition),

        # Haryana 2019:
        # Source
        # https://en.wikipedia.org/wiki/2019_Haryana_Legislative_Assembly_election
        coalition = ifelse((party == "BJP" | party == "JNJP" | party == "IND")
            & year == 2019 & state_name == "Haryana", 1, coalition),
        coalition = ifelse(party != "BJP" & party != "JNJP" & party != "IND" &
            year == 2019 & state_name == "Haryana", 0, coalition),

        # Jharkhand 2019:
        # Source
        # https://en.wikipedia.org/wiki/2019_Jharkhand_Legislative_Assembly_election
        coalition = ifelse((party == "JMM" | party == "INC" | party == "RJD") &
            year == 2019 & state_name == "Jharkhand", 1, coalition),
        coalition = ifelse(party != "JMM" & party != "INC" & party != "RJD" &
            year == 2019 & state_name == "Jharkhand", 0, coalition),

        # Maharashtra 2019:
        # Source:
        # https://en.wikipedia.org/wiki/2019_Maharashtra_Legislative_Assembly_election
        # https://en.wikipedia.org/wiki/Maha_Vikas_Aghadi
        coalition = ifelse((party == "SHS" | party == "NCP" | party == "INC" |
            party == "BVA" | party == "SP" | party == "PJP" | party == "SWP" |
            party == "PWPI") & year == 2019 & state_name == "Maharashtra", 1,
            coalition),
        coalition = ifelse(party != "SHS" & party != "NCP" & party != "INC" &
            party != "BVA" & party != "SP" & party != "PJP" & party != "SWP" &
            party != "PWPI" & year == 2019 & state_name == "Maharashtra", 0,
            coalition),

        # Odisha 2019:
        # Source
        # https://en.wikipedia.org/wiki/2019_Odisha_Legislative_Assembly_election
        coalition = ifelse(party == "BJD" & year == 2019 &
            state_name == "Odisha", 1, coalition),
        coalition = ifelse(party != "BJD" & year == 2019 &
            state_name == "Odisha", 0, coalition),

        # Sikkim 2019:
        # Source
        # https://en.wikipedia.org/wiki/2019_Sikkim_Legislative_Assembly_election
        coalition = ifelse(party == "SKM" & year == 2019 &
            state_name == "Sikkim", 1, coalition),
        coalition = ifelse(party != "SKM" & year == 2019 &
            state_name == "Sikkim", 0, coalition),

        # Delhi 2020:
        coalition = ifelse(party == "AAP" & year == 2020 &
            state_name == "Delhi", 1, coalition),
        coalition = ifelse(party != "AAP" & year == 2020 &
            state_name == "Delhi", 0, coalition),

        # Bihar 2020:
        # Source
        # https://en.wikipedia.org/wiki/2020_Bihar_Legislative_Assembly_election
        coalition = ifelse((party == "JD(U)" | party == "BJP" | party == "VIP"
            | party == "HAM") & year == 2020 & state_name == "Bihar", 1,
            coalition),
        coalition = ifelse(party != "JD(U)" & party != "BJP" & party != "VIP"
            & party != "HAM" & year == 2020 & state_name == "Bihar", 0,
            coalition)
    ) %>%
    select(-unique_id, -party, -election_number, -merge) %>%
    filter(year >= 2000)

# Saving ######################################################################

saveRDS(df_elections, file = here("data/clean/coalition_parties.rds"))
