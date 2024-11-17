library(dplyr)
library(stringr)
library(ggplot2)
library(fixest)
library(data.table)
library(broom)
library(dotwhisker)
library(modelsummary)
options(modelsummary_format_numeric_latex = "plain")
library(tinytable)
library(kableExtra)
library(patchwork)
library(rio)
library(here)

runLM <- function(depvar, indvar, dataframe){
    regformula <- as.formula(paste0(depvar, "~ ", indvar))
    results <- lm(regformula, data = dataframe)
    print(summary(results))
    
    resultsDF <- tidy(results) %>%
        mutate(dependent_variable = depvar)

    return(resultsDF)
}

# Opposite of %in% function:
'%!in%' <- function(x, y)!('%in%'(x, y))

# Function to cleaning geographic identifiers in IHDS #########################

clean_identifiers <- function(df) {
    df_return <- df %>%
        rename(district_code = DISTID) %>%
        mutate(
            state_name = str_sub(STATEID, 6, -3),
            state_name = str_trim(state_name),
            state_name = str_to_lower(state_name),
            state_name = gsub(" ", "", state_name),
            state_name = ifelse(state_name == "uttarakhand", "uttaranchal",
            state_name),

            district_code = case_when(
                state_name == "pondicherry" & district_code == 0 ~ 2,
                state_name == "sikkim" & district_code == 0 ~ 4,
                state_name == "mizoram" & district_code == 0 ~ 3,
                TRUE ~ district_code
            ),

            # Generate unique ID for schools:
            state = substr(STATEID, (nchar(as.character(STATEID)) - 2),
                nchar(as.character(STATEID))),
            district = as.character(ifelse(nchar(district_code) == 1,
                paste0("0", district_code), district_code)),
            village = as.character(ifelse(nchar(PSUID) == 1,
                paste0("0", PSUID), PSUID)),

            id_psu = as.numeric(paste0(state, district, village)),
            school_id = as.numeric(paste0(state, district, village, SCHOOLID))
        ) %>%
        # Dropping UTs that don't hold legislative assembly elections
        filter(state_name != "chandigarh") %>%
        filter(state_name != "daman&diu") %>%
        filter(state_name != "dadra+nagarhaveli")

    return(df_return)
}

# Function to clean incorrect IHDS Survey dates ###############################

clean_ihds_survey_dates <- function(df) {
	df_return <- df %>%
		rename(
			day_survey = SC9D,
	        month_survey = SC9M,
	        year_survey = SC9Y,
		) %>%
		mutate(
			day_survey = case_when(
				id_psu == 232201 ~ 9,
				TRUE ~ day_survey
			),
			day_survey = ifelse(day_survey %in% NA, 1, day_survey),
			month_survey = case_when(
				id_psu == 190611 ~ 3,
				id_psu == 232201 ~ 6,
				TRUE ~ month_survey
			),
			year_survey = year_survey + 2000,
			year_survey = case_when(
				id_psu == 270703 ~ 2012,
				id_psu == 232201 ~ 2012,
				TRUE ~ year_survey
			),

	        date_survey = dmy(paste(day_survey, month_survey, year_survey,
	            sep = "/")),
		)

	return(df_return)
}

# Function to clean IHDS relative to election years ###########################

clean_ihds_election_years <- function(df) {
	df_return <- df %>%
		mutate(
			years_to_election_no_by = case_when(
	            district_name == "purbachamparan" ~ "1",
	            district_name == "muzaffarpur" ~ "1",
	            district_name == "siwan" ~ "1",
	            district_name == "saran" ~ "1",
	            state_name == "andhrapradesh" & district_name == "krishna" ~ "-1",
	            state_name == "andhrapradesh" & district_name == "guntur" ~ "-1",
	            state_name == "andhrapradesh" & district_name == "prakasam" ~ "-1",
	            state_name == "andhrapradesh" & district_name == "nellore" ~ "-1",
	            state_name == "andhrapradesh" & district_name == "cuddapah" ~ "-1",
	            state_name == "andhrapradesh" & district_name == "kurnool" ~ "-1",
	            state_name == "andhrapradesh" & district_name == "anantapur" ~ "-1",
	            state_name == "andhrapradesh" & district_name == "chittoor" ~ "-1",
	            state_name == "bihar" & district_name == "madhubani" & (date_survey >= "2010-10-21" & date_survey < "2011-10-21") ~ "1",
	            state_name == "bihar" & district_name == "madhubani" & (date_survey > "2011-10-21" & date_survey < "2012-10-21") ~ "2",
	            state_name == "bihar" & district_name == "bhagalpur" & (date_survey >= "2010-11-01" & date_survey < "2011-11-01") ~ "1",
	            state_name == "bihar" & district_name == "bhagalpur" & (date_survey >= "2011-11-01" & date_survey < "2012-11-01") ~ "2",
	            state_name == "bihar" & district_name == "nalanda" & (date_survey >= "2010-11-09" & date_survey < "2011-11-09") ~ "1",
	            state_name == "bihar" & district_name == "nalanda" & (date_survey >= "2011-11-09" & date_survey < "2012-11-09") ~ "2",
	            state_name == "bihar" & district_name == "patna" & (date_survey >= "2010-11-01" & date_survey < "2011-11-01") ~ "1",
	            state_name == "bihar" & district_name == "patna" & (date_survey >= "2011-11-01" & date_survey < "2012-11-01") ~ "2",
	            state_name == "bihar" & district_name == "buxar" & (date_survey >= "2010-11-20" & date_survey < "2011-11-20") ~ "1",
	            state_name == "bihar" & district_name == "buxar" & (date_survey >= "2011-11-20" & date_survey < "2012-11-20") ~ "2",
	            state_name == "bihar" & district_name == "kaimur(bhabua)" & (date_survey >= "2010-11-20" & date_survey < "2011-11-20") ~ "1",
	            state_name == "bihar" & district_name == "kaimur(bhabua)" & (date_survey >= "2011-11-20" & date_survey < "2012-11-20") ~ "2",
	            state_name == "bihar" & district_name == "rohtas" & (date_survey >= "2010-11-20" & date_survey < "2011-11-20") ~ "1",
	            state_name == "bihar" & district_name == "rohtas" & (date_survey >= "2011-11-20" & date_survey < "2012-11-20") ~ "2",
	            state_name == "bihar" & district_name == "gaya" & (date_survey >= "2010-11-09" & date_survey < "2011-11-09") ~ "1",
	            state_name == "bihar" & district_name == "gaya" & (date_survey >= "2011-11-09" & date_survey < "2012-11-09") ~ "2",
	            state_name == "haryana" & district_name == "panchkula" ~ "-2",
	            state_name == "haryana" & district_name == "ambala" ~ "-2",
	            state_name == "haryana" & district_name == "sonipat" ~ "-2",
	            state_name == "haryana" & district_name == "faridabad" ~ "-2",
	            state_name == "jharkhand" & district_name == "palamu" ~ "2",
	            state_name == "jharkhand" & district_name == "dhanbad" ~ "2",
	            state_name == "jharkhand" & district_name == "bokaro" ~ "2",
	            state_name == "jharkhand" & district_name == "ranchi" ~ "2",
	            state_name == "jharkhand" & district_name == "pashchimisinghbhum" ~ "2",
	            state_name == "jharkhand" & district_name == "purbisinghbhum" ~ "2",
	            state_name == "kerala" & (date_survey > "2011-04-13" & date_survey <= "2012-04-13") ~ "1",
	            state_name == "kerala" & (date_survey > "2012-04-13" & date_survey <= "2013-04-13") ~ "2",
	            state_name == "maharashtra" & (date_survey > "2011-10-14" & date_survey <= "2012-10-15") ~ "-2",
	            state_name == "pondicherry" & (date_survey > "2011-04-13" & date_survey <= "2012-04-13") ~ "1",
	            state_name == "pondicherry" & (date_survey > "2012-04-13" & date_survey <= "2013-04-13") ~ "2",
	            state_name == "punjab" & (date_survey >= "2012-01-31" & date_survey < "2013-01-30") ~ "1",
	            state_name == "tamilnadu" & (date_survey >= "2011-04-14" & date_survey < "2012-04-14") ~ "1",
	            state_name == "tamilnadu" & (date_survey >= "2012-04-14" & date_survey < "2013-04-14") ~ "2",
	            state_name == "westbengal" & district_name == "birbhum" & (date_survey > "2011-04-22" & date_survey <= "2012-04-22") ~ "1",
	            state_name == "westbengal" & district_name == "birbhum" & (date_survey > "2012-04-22" & date_survey <= "2013-04-22") ~ "2",
	            state_name == "westbengal" & (district_name == "darjiling" | district_name == "jalpaiguri" | district_name == "maldah" | district_name == "uttardinajpur") & (date_survey > "2011-04-18" & date_survey <= "2012-04-18") ~ "1",
	            state_name == "westbengal" & (district_name == "darjiling" | district_name == "jalpaiguri" | district_name == "maldah" | district_name == "uttardinajpur") & (date_survey > "2012-04-18" & date_survey <= "2013-04-18") ~ "2",
	            state_name == "westbengal" & (district_name == "haora" | district_name == "hugli") & (date_survey > "2011-05-03" & date_survey <= "2012-05-03") ~ "1",
	            state_name == "westbengal" & (district_name == "haora" | district_name == "hugli") & (date_survey > "2012-05-03" & date_survey <= "2013-05-03") ~ "2",
	            state_name == "westbengal" & (district_name == "kolkata" | district_name == "southtwentyfourparganas") & (date_survey > "2011-04-27" & date_survey <= "2012-04-27") ~ "1",
	            state_name == "westbengal" & (district_name == "kolkata" | district_name == "southtwentyfourparganas") & (date_survey > "2012-04-27" & date_survey <= "2013-04-27") ~ "2",
	            state_name == "westbengal" & (district_name == "murshidabad" | district_name == "nadia") & (date_survey > "2011-04-23" & date_survey <= "2012-04-23") ~ "1",
	            state_name == "westbengal" & (district_name == "murshidabad" | district_name == "nadia") & (date_survey > "2012-04-23" & date_survey <= "2013-04-23") ~ "2",
	            TRUE ~ years_to_election_no_by
			)
		)

	return(df_return)
}

# Function to plot event study plots ##########################################

tidyCyclePlots <- function(model, interaction, log_outcome) {

	if(interaction) {
		df <- tidy(model) %>%
			filter(str_detect(term, ":share_constituencies_election")) %>%
			mutate(
				term = gsub(":share_constituencies_election", "", term),
				term = gsub("_all", "", term),
				term = ifelse(term == "years_to_election-2", "-2 or more", term),
				term = ifelse(term == "years_to_election2", "2 or more", term),
				term = gsub("years_to_election", "", term),
				term = ifelse(term == "0", "Election Year", term),
				order = ifelse(term == "-2 or more", 5, 0),
				order = ifelse(term == "-1", 4, order),
				order = ifelse(term == "Election Year", 3, order),
				order = ifelse(term == "1", 2, order),
				order = ifelse(term == "2 or more", 1, order)
			) %>%
			add_row(
				term = "Election Year", estimate = 0, std.error = 0,
					statistic = 0, p.value = 0, order = 3, .before = 3
			) %>%
			arrange(order) %>%
			select(-order)
	}
	else {
		df <- tidy(model) %>%
			filter(
				str_detect(term, "vs_year_") |
				str_detect(term, "years_to_election")
			) %>%
			mutate(
				term = case_when(
					term == "vs_year_0" ~ "-2 or more",
					term == "years_to_election-2" ~ "-2 or more",
					term == "vs_year_1" ~ "-1",
					term == "years_to_election-1" ~ "-1",
					term == "vs_year_3" ~ "1",
					term == "years_to_election1" ~ "1",
					term == "vs_year_4" ~ "2 or more",
					term == "years_to_election2" ~ "2 or more",
					TRUE ~ term
				),
				order = ifelse(term == "-2 or more", 5, 0),
				order = ifelse(term == "-1", 4, order),
				order = ifelse(term == "1", 2, order),
				order = ifelse(term == "2 or more", 1, order)
			) %>%
			add_row(
				term = "Election Year", estimate = 0, std.error = 0,
					statistic = 0, p.value = 0, order = 3, .before = 3
			) %>%
			arrange(order) %>%
			select(-order)
	}

	if(log_outcome) {
		plot <- dwplot(df,
			conf.level = .95,
			vline = geom_vline(xintercept = 0, linetype = 2)
			) +
			theme_bw() +
		    scale_colour_grey(start = .1, end = .1) +
			coord_flip() +
			geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
		    labs(
		        y = "Years to Election",
		        x = "Log Absence per Teacher",
		        color = NULL
		    ) +
		    theme(
		        legend.position = "none",
		        legend.background = element_blank(),
		        panel.background = element_rect(fill = "transparent"),
		        panel.border = element_blank(),
		        panel.grid.minor = element_blank(),
		        panel.grid.major.x = element_blank(),
		        plot.background = element_rect(fill = "transparent",
		            color = NA),
		)
	}

	else {
		plot <- dwplot(df,
			conf.level = .95,
			vline = geom_vline(xintercept = 0, linetype = 2)
			) +
			theme_bw() +
		    scale_colour_grey(start = .1, end = .1) +
			coord_flip() +
			geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
		    labs(
		    	y = "Years to Election",
		        x = "Probability Absent",
		        color = NULL
		    ) +
		    theme(
		        legend.position = "none",
		        legend.background = element_blank(),
		        panel.background = element_rect(fill = "transparent"),
		        panel.border = element_blank(),
		        panel.grid.minor = element_blank(),
		        panel.grid.major.x = element_blank(),
		        plot.background = element_rect(fill = "transparent",
		            color = NA),
		)
	}

	return(plot)
}

# Event Study Model Function ##################################################

eventStudy <- function(
	government, # Character for government or private schools?
	by_election = FALSE, # Logical vector for whether we subset to by-elections only
	event_study = TRUE, # Logical vector specifying whether model is event study
	# or uses election year dummy
	df # Data frame
	) {

	# Setting-up locals #######################################################

	if (government == "government") {
		management_type_text <- "government"
		management_type_abbreviation <- "g"
		management_type_title <- "Government"
		private_dummy <- 0
		# Subset data
		df <- df[private == private_dummy, ]
	} else if (government == "private") {
		management_type_text <- "private"
		management_type_abbreviation <- "p"
		management_type_title <- "Private"
		private_dummy <- 1
		# Subset data
		df <- df[private == private_dummy, ]
	} else {
		management_type_text <- "testrun"
		management_type_abbreviation <- "tr"
		management_type_title <- "Test Run"
		private_dummy <- 0
		df <- import(
			here("data/clean/absence_src_clean_sample.rds")
		)
	}

	if (by_election == TRUE) {
		by_election_text <- "byelection"
		by_election_type <- "be"
		by_election_type_title <- " For By-Elections"
		df <- df %>%
			filter(
				by_election == 1 |
				previous_by_election == 1 |
				next_by_election == 1
			)
	} else{
		"Analysis on full sample"
		by_election_text <- ""
		by_election_type <- ""
		by_election_type_title <- ""
	}	

	# Dummy for absent, no teacher controls ###################################

	if (event_study == TRUE) {
		formula <- as.formula(
			absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
			lag_absent
		)
		formula_no_lag <- as.formula(
			absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural
		)
		cycle <- "cycle"
	} else {
		formula <- as.formula(
			absent ~ vs_year_2 + rural + lag_absent
		)
		formula_no_lag <- as.formula(
			absent ~ vs_year_2 + rural
		)
		cycle <- ""
	}

	# No Fixed Effects
	lm_no_fe <- feols(formula,
	    cluster = c("unique_constituency_code", "year"),
	    data = df
	)

	# Year fixed effects
	lm_year_fe <- feols(formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "year",
	    data = df
	)

	# School fixed effects
	lm_school_fe <- feols(formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "school_code",
	    data = df
	)

	n_school_fe <- format(
		lm_school_fe[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# School + Year Fixed Effects
	lm_year_school_fe <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)
	
	n_year_school_fe <- format(
		lm_year_school_fe[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# No lag
	lm_year_school_fe_no_lag <- feols(
		formula_no_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)
	
	n_year_school_fe_no_lag <- format(
		lm_year_school_fe_no_lag[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# Dummy for absent, teacher controls ######################################

	if (event_study == TRUE) {
		formula <- as.formula(
			absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
			lag_absent + teachers
		)
		formula_no_lag <- as.formula(
			absent ~ vs_year_0 + vs_year_1 + vs_year_3 + vs_year_4 + rural +
			teachers
		)
		cycle <- "cycle"
	} else {
		formula <- as.formula(
			absent ~ vs_year_2 + rural + lag_absent + teachers
		)
		formula_no_lag <- as.formula(
			absent ~ vs_year_2 + rural + teachers
		)
		cycle <- ""
	}

	# No Fixed Effects
	lm_no_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    data = df
	)

	# Year fixed effects
	lm_year_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "year",
	    data = df
	)

	# School fixed effects
	lm_school_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "school_code",
	    data = df
	)
	
	n_school_fe_teachers <- format(
		lm_school_fe_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# School + Year Fixed Effects
	lm_year_school_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)
	
	n_year_school_fe_teachers <- format(
		lm_year_school_fe_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# No lag
	lm_year_school_fe_no_lag_teachers <- feols(
		formula_no_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)
	
	n_year_school_fe_no_lag_teachers <- format(
		lm_year_school_fe_no_lag_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# Exporting models of absence dummy  ######################################

	models_list <- list(
		lm_no_fe, lm_year_fe, lm_school_fe, lm_year_school_fe,
		lm_year_school_fe_no_lag, lm_no_fe_teachers, lm_year_fe_teachers,
		lm_school_fe_teachers, lm_year_school_fe_teachers,
		lm_year_school_fe_no_lag_teachers
	)

	f <- function(x) format(round(x, 3), big.mark = ",")
	gof <- list(
	    list("raw" = "fe: year", "clean" = "Year FE", "fmt" = f),
	    list("raw" = "fe: school_code", "clean" = "School FE", "fmt" = f),
	    list("raw" = "nobs", "clean" = "Observations", "fmt" = f)
	)

	rows <- tribble(
	    ~term, ~nofe, ~yearfe, ~schoolfe, ~yearschoolfe, ~nolag, ~nofeteachers, ~yearfeteachers, ~schoolfeteachers, ~yearschoolfeteachers, ~nolagteachers,
	    "Year FE", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes",
	    "School FE", "No", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes",
	    "Lagged DV", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "No",
	    "Teacher Controls", "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes",
	    "Number of Schools", n_school_fe, n_school_fe, n_school_fe, n_year_school_fe, n_year_school_fe_no_lag, n_school_fe_teachers, n_school_fe_teachers, n_school_fe_teachers, n_year_school_fe_teachers, n_year_school_fe_no_lag_teachers
	)
	attr(rows, "position") <- c(9, 10, 11, 12, 13)

	if (event_study == TRUE) {
		cycle_title <- " over the Electoral Cycle "
		cycle_name_table <- "cycle"
		label_title <- "appendix"
		model <- "extensiveakhmedov"
		coef_map <- c(
			"vs_year_0" = "2 or More Years Before Election",
	        "vs_year_1" = "1 Year Before Election",
	        "vs_year_3" = "1 Year After Election",
	        "vs_year_4" = "2 or More Years After Election"
	    )
	    figure_reference <- paste0(
	    	" Column 9 corresponds to Panel A of Figure \\ref{fig:",
	    	management_type_abbreviation,
	    	by_election_type,
	    	"extensiveakhmedov}."
	    )
	} else {
		cycle_title <- " "
		cycle_name_table <- ""
		label_title <- ""
		model <- "extensive"
		coef_map <- c("vs_year_2" = "Election Year")
		figure_reference <- ""
	}

	table_note <- paste0(
		"\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses. The dependent variable is a dummy variable that takes the value of one if the school reports any teacher absenteeism in that year. Each specification includes a dummy for whether the school is in a rural area. Columns 1-4 and 6-9 also include a lagged dependent variable. The election year mean is \\input{data/output/text/electionyearmeandummycycle",
		management_type_text,
		by_election_text,
		".tex}.",
		figure_reference
	)

	any_absence_dise_table <- modelsummary(models_list,
	    title = paste0(
	    	"Any Absence",
	    	cycle_title,
	    	"in ",
	    	management_type_title,
	    	" Schools Using DISE Data",
	    	by_election_type_title,
	    	"\\label{",
	    	label_title,
	    	"table:",
	    	management_type_abbreviation,
	    	model,
	    	by_election_type,
	    	"}"
	    ),
	    coef_map = coef_map,
	    stars = c("*" = .1, "**" = .05, "***" = .01),
	    estimate = "{estimate}{stars}",
	    gof_map = gof,
	    add_rows = rows,
	    notes = table_note,
	    output = "tinytable",
	    escape = FALSE
	) %>%
	group_tt(j = list("Absent" = 2:11)) %>%
	style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
	save_tt(
		here(
	    	paste0(
	    		"data/output/tables/anyabsence",
	    		cycle_name_table,
	    		management_type_text,
	    		by_election_text,
	    		".tex"
	    	)
	    ),
	    overwrite = TRUE
	)

	if (by_election) {

		table_note <- paste0(
			"\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses. The dependent variable is a dummy variable that takes the value of one if the school reports any teacher absenteeism in that year. Each specification includes a dummy for whether the school is in a rural area. Columns 1-4 and 6-9 also include a lagged dependent variable. The election year mean is \\input{data/output/text/electionyearmeandummycycle",
			management_type_text,
			by_election_text,
			".tex}.",
			figure_reference
		)

		any_absence_dise_table <- modelsummary(models_list,
		    title = paste0(
		    	"Any Absence",
		    	cycle_title,
		    	"in ",
		    	management_type_title,
		    	" Schools Using DISE Data",
		    	by_election_type_title,
		    	"\\label{",
		    	label_title,
		    	"table:",
		    	management_type_abbreviation,
		    	model,
		    	by_election_type,
		    	"}"
		    ),
		    coef_map = coef_map,
		    stars = c("*" = .1, "**" = .05, "***" = .01),
		    estimate = "{estimate}{stars}",
		    gof_map = gof,
		    add_rows = rows,
		    notes = table_note,
		    output = "tinytable",
		    escape = FALSE
		) %>%
		group_tt(j = list("Absent" = 2:11)) %>%
		style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
		save_tt(
			here(
		    	paste0(
		    		"data/output/tables/anyabsence",
		    		cycle_name_table,
		    		management_type_text,
		    		by_election_text,
		    		".tex"
		    	)
		    ),
		    overwrite = TRUE
		)

	}
	else {
		print("No by-election")
	}

	# Footer Material for any absence #########################################

	# Reference Category Mean:
	sink(
		file = here(
			paste0(
				"data/output/text/electionyearmeandummy",
				cycle,
				management_type_text,
				by_election_text,
				".tex"
			)
		)
	)

	cat(
		round(
			mean(
				df$absent[df$private == private_dummy &
				df$vs_year_2 == 1],
				na.rm = TRUE
			),
			3
		)
	)
	
	sink(file = NULL)

	if(by_election) {
		sink(
			file = here(
				paste0(
					"data/output/text/electionyearmeandummy",
					cycle,
					management_type_text,
					by_election_text,
					".tex"
				)
			)
		)

		cat(
			round(
				mean(
					df$absent[df$private == private_dummy &
					df$vs_year_2 == 1],
					na.rm = TRUE
				),
				3
			)
		)
		
		sink(file = NULL)
	}
	else {
		print("Not by-election")
	}

	# Sample sizes for figures

	if (event_study) {
		sink(file = 
			here(
				paste0(
					"data/output/text/nschools",
					management_type_text,
					by_election_type,
					".tex"
				)
			)
		)
		cat(n_year_school_fe)
		sink(file = NULL)

		sink(file = 
			here(
				paste0(
					"data/output/text/nobs",
					management_type_text,
					by_election_type,
					".tex"
				)
			)
		)
		cat(format(lm_year_school_fe_teachers$nobs, big.mark = ","))
		sink(file = NULL)

		if (by_election) {
			sink(file = 
				here(
					paste0(
						"data/output/text/nschools",
						management_type_text,
						by_election_type,
						".tex"
					)
				)
			)
			cat(n_year_school_fe)
			sink(file = NULL)

			sink(file = 
				here(
					paste0(
						"data/output/text/nobs",
						management_type_text,
						by_election_type,
						".tex"
					)
				)
			)
			cat(format(lm_year_school_fe_teachers$nobs, big.mark = ","))
			sink(file = NULL)

		}
		else {
			print("Not by-election")
		}

	}
	else {
		print("Not event study, no materials for figures")
	}

	# Log absence models, no teacher controls ##############################

	if (event_study == TRUE) {
		formula <- as.formula(
			ihs_average_absence ~ vs_year_0 + vs_year_1 + vs_year_3 +
			vs_year_4 + rural + lag_absent
		)
		formula_no_lag <- as.formula(
			ihs_average_absence ~ vs_year_0 + vs_year_1 + vs_year_3 +
			vs_year_4 + rural
		)
		cycle <- "cycle"
	} else {
		formula <- as.formula(
			ihs_average_absence ~ vs_year_2 + rural + lag_absent
		)
		formula_no_lag <- as.formula(
			ihs_average_absence ~ vs_year_2 + rural
		)
		cycle <- ""
	}

	# No Fixed Effects
	log_no_fe <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    data = df
	)

	# Year fixed effects
	log_year_fe <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "year",
	    data = df
	)

	# School fixed effects
	log_school_fe <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "school_code",
	    data = df
	)

	n_school_fe <- format(
		log_school_fe[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# School + Year Fixed Effects
	log_year_school_fe <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)
	
	n_year_school_fe <- format(
		log_year_school_fe[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# No lag
	log_year_school_fe_no_lag <- feols(
		formula_no_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)

	n_year_school_fe_no_lag <- format(
		log_year_school_fe_no_lag[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# Dummy for log absence, teacher controls #################################

	if (event_study == TRUE) {
		formula <- as.formula(
			ihs_average_absence ~ vs_year_0 + vs_year_1 + vs_year_3 +
			vs_year_4 + rural + lag_absent + teachers
		)
		formula_no_lag <- as.formula(
			ihs_average_absence ~ vs_year_0 + vs_year_1 + vs_year_3 +
			vs_year_4 + rural + teachers
		)
		cycle <- "cycle"
	} else {
		formula <- as.formula(
			ihs_average_absence ~ vs_year_2 + rural + lag_absent + teachers
		)
		formula_no_lag <- as.formula(
			ihs_average_absence ~ vs_year_2 + rural + teachers
		)
		cycle <- ""
	}

	# No Fixed Effects
	log_no_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    data = df
	)

	# Year fixed effects
	log_year_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "year",
	    data = df
	)

	# School fixed effects
	log_school_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "school_code",
	    data = df
	)
	
	n_school_fe_teachers <- format(
		log_school_fe_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# School + Year Fixed Effects
	log_year_school_fe_teachers <- feols(
		formula,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)
	
	n_year_school_fe_teachers <- format(
		log_year_school_fe_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# No lag
	log_year_school_fe_no_lag_teachers <- feols(
		formula_no_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = df
	)
	
	n_year_school_fe_no_lag_teachers <- format(log_year_school_fe_no_lag_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# Exporting models of log absence #########################################

	models_list <- list(
		log_no_fe, log_year_fe, log_school_fe, log_year_school_fe,
		log_year_school_fe_no_lag, log_no_fe_teachers, log_year_fe_teachers,
		log_school_fe_teachers, log_year_school_fe_teachers,
		log_year_school_fe_no_lag_teachers
	)

	f <- function(x) format(round(x, 3), big.mark = ",")
	gof <- list(
	    list("raw" = "fe: year", "clean" = "Year FE", "fmt" = f),
	    list("raw" = "fe: school_code", "clean" = "School FE", "fmt" = f),
	    list("raw" = "nobs", "clean" = "Observations", "fmt" = f)
	)

	rows <- tribble(
	    ~term, ~nofe, ~yearfe, ~schoolfe, ~yearschoolfe, ~nolag, ~nofeteachers, ~yearfeteachers, ~schoolfeteachers, ~yearschoolfeteachers, ~nolagteachers,
	    "Year FE", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes",
	    "School FE", "No", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes",
	    "Lagged DV", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "No",
	    "Teacher Controls", "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes",
	    "Number of Schools", n_school_fe, n_school_fe, n_school_fe, n_year_school_fe, n_year_school_fe_no_lag, n_school_fe_teachers, n_school_fe_teachers, n_school_fe_teachers, n_year_school_fe_teachers, n_year_school_fe_no_lag_teachers
	)

	if (event_study == TRUE) {
		attr(rows, "position") <- c(9, 10, 11, 12, 13)
		cycle_title <- " over the Electoral Cycle "
		cycle_name_table <- "cycle"
		label_title <- "appendix"
		model <- "intensiveakhmedov"
		coef_map <- c(
			"vs_year_0" = "2 or More Years Before Election",
	        "vs_year_1" = "1 Year Before Election",
	        "vs_year_3" = "1 Year After Election",
	        "vs_year_4" = "2 or More Years After Election"
	    )
	    figure_reference <- paste0(
			" Column 9 corresponds to Panel B of Figure \\ref{fig:",
			management_type_abbreviation,
			by_election_type,
			"extensiveakhmedov}."
		)
	} else {
		attr(rows, "position") <- c(3, 4, 5, 6, 7)
		cycle_title <- " "
		cycle_name_table <- ""
		label_title <- "appendix"
		model <- "intensive"
		coef_map <- c("vs_year_2" = "Election Year")
		figure_reference <- ""
	}

	table_note <- paste0(
    	"\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses. The dependent variable is the log number off average absences per school. Each specification includes a dummy for whether the school is in a rural area. Columns 1-4 and 6-9 also include a lagged dependent variable. The election year mean is \\input{data/output/text/electionyearmeanlogcycle",
    	management_type_text,
    	by_election_text,
    	".tex}.",
    	figure_reference
    )

	log_absence_dise_table <- modelsummary(
		models_list,
	    title = paste0(
	    	"Log Average Absence in ",
	    	management_type_title,
	    	" Schools over the Electoral Cycle Using DISE Data",
	    	by_election_type_title,
	    	"\\label{",
	    	label_title,
	    	"table:",
	    	management_type_abbreviation,
	    	model,
	    	by_election_type,
	    	"}"
	    ),
	    coef_map = coef_map,
	    stars = c("*" = .1, "**" = .05, "***" = .01),
	    estimate = "{estimate}{stars}",
	    gof_map = gof,
	    add_rows = rows,
	    notes = table_note,
	    output = "tinytable",
	    escape = FALSE
	) %>%
	group_tt(j = list("Log Average Absence" = 2:11)) %>%
	style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
	save_tt(
		here(
	    	paste0(
	    		"data/output/tables/logabsence",
	    		cycle_name_table,
	    		management_type_text,
	    		by_election_text,
	    		".tex"
	    	)
	    ),
	    overwrite = TRUE
	)

	if (by_election) {

		table_note <- paste0(
	    	"\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses. The dependent variable is the log number off average absences per school. Each specification includes a dummy for whether the school is in a rural area. Columns 1-4 and 6-9 also include a lagged dependent variable. The election year mean is \\input{data/output/text/electionyearmeanlogcycle",
	    	management_type_text,
	    	by_election_text,
	    	".tex}.",
	    	figure_reference
	    )

		log_absence_dise_table <- modelsummary(
			models_list,
		    title = paste0(
		    	"Log Average Absence in ",
		    	management_type_title,
		    	" Schools over the Electoral Cycle Using DISE Data",
		    	by_election_type_title,
		    	"\\label{",
		    	label_title,
		    	"table:",
		    	management_type_abbreviation,
		    	model,
		    	by_election_type,
		    	"}"
		    ),
		    coef_map = coef_map,
		    stars = c("*" = .1, "**" = .05, "***" = .01),
		    estimate = "{estimate}{stars}",
		    gof_map = gof,
		    add_rows = rows,
		    notes = table_note,
		    output = "tinytable",
		    escape = FALSE
		) %>%
		group_tt(j = list("Log Average Absence" = 2:11)) %>%
		style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
		save_tt(
			here(
		    	paste0(
		    		"data/output/tables/logabsence",
		    		cycle_name_table,
		    		management_type_text,
		    		by_election_text,
		    		".tex"
		    	)
		    ),
		    overwrite = TRUE
		)

	}
	else {
		print("No by-election")
	}

	# Getting Footer Material #################################################

	# Reference Category Mean:
	log_absence <- mean(
		df$ihs_average_absence[df$private == private_dummy & df$vs_year_2 == 1],
		na.rm = TRUE,
		3
	)

	mean_absence <- round(10 ^ (log_absence), 3)

	sink(
		file = here(
			paste0(
				"data/output/text/electionyearmeanlogcycle",				
				management_type_text,
				by_election_text,
				".tex"
			)
		)
	)
	cat(mean_absence)
	sink(file = NULL)

	if (by_election) {
		sink(
			file = here(
				paste0(
					"data/output/text/electionyearmeanlogcycle",				
					management_type_text,
					by_election_text,
					".tex"
				)
			)
		)
		cat(mean_absence)
		sink(file = NULL)
	}
	else {
		print("Not by_election")
	}

	# Sample sizes for figures

	if (event_study) {
		sink(file = 
			here(
				paste0(
					"data/output/text/nschools",
					management_type_text,
					by_election_type,
					"log.tex"
				)
			)
		)
		cat(n_year_school_fe)
		sink(file = NULL)

		sink(file = 
			here(
				paste0(
					"data/output/text/nobs",
					management_type_text,
					by_election_type,
					"log.tex"
				)
			)
		)
		cat(format(lm_year_school_fe_teachers$nobs, big.mark = ","))
		sink(file = NULL)

		if (by_election) {
			sink(file = 
				here(
					paste0(
						"data/output/text/nschools",
						management_type_text,
						by_election_type,
						"log.tex"
					)
				)
			)
			cat(n_year_school_fe)
			sink(file = NULL)

			sink(file = 
				here(
					paste0(
						"data/output/text/nobs",
						management_type_text,
						by_election_type,
						"log.tex"
					)
				)
			)
			cat(format(lm_year_school_fe_teachers$nobs, big.mark = ","))
			sink(file = NULL)
		}
		else {
			print("Not by-election")
		}

	}
	else {
		print("Not event study, no materials for figures")
	}

	if (event_study == TRUE) {

		linear_cycle <- tidyCyclePlots(
			lm_year_school_fe_no_lag,
			interaction = FALSE,
			log_outcome = FALSE
		)

		linear_cycle <- linear_cycle + labs(x = "P(Absent)")

		log_cycle <- tidyCyclePlots(
			log_year_school_fe_no_lag,
			interaction = FALSE,
			log_outcome = TRUE
		)

		plots <- list(linear_cycle, log_cycle)

		plot <- wrap_plots(plots, ncol = 1, axes = "collect") +
			plot_layout(axes = "collect_x") +
    		plot_annotation(tag_levels = 'A')

		ggsave(
			here(
				paste0(
					"data/output/figures/anyabsencecycle",
					management_type_text,
					by_election_text,
					".pdf"
				)
			),
			plot,
			height = 4.5,
			width = 6.5,
			units = "in",
		    bg = "transparent"
		)

		if (by_election) {
			ggsave(
				here(
					paste0(
						"data/output/figures/anyabsencecycle",
						management_type_text,
						by_election_text,
						".pdf"
					)
				),
				plot,
				height = 4.5,
				width = 6.5,
				units = "in",
			    bg = "transparent"
			)

		}
		else {
			print("No by-election")
		}

	}
	else {
		print("No figure plotted as not event study model")
	}

}

# Function to clean election general election data from TCPD data #############

clean_elections <- function(df) {

	df <- df %>%
		rename(st_code_ec = state_code_ec) %>%
		mutate(
			year = case_when(
	            state_name == "Nagaland" & assembly_no == 9 & constituency_no == 37 & poll_no == 1 & year == 0 ~ 2000,
	            state_name == "Punjab" & assembly_no == 11 & constituency_no == 38 & poll_no == 1 & year == 0 ~ 2000,
	            TRUE ~ year
	        ),
	        constituency_name = gsub("\\(SC)", "", constituency_name),
	        constituency_name = gsub("\\(ST)", "", constituency_name),
	        constituency_name = str_to_title(constituency_name),
	        constituency_name = str_trim(constituency_name),
	        constituency_no = as.integer(constituency_no),
	        month = case_when(
	            state_name == "Himachal Pradesh" & assembly_no == 9 & constituency_no == 43 & poll_no == 1 & year == 1998 ~ 11,
	            state_name == "Goa" & assembly_no == 4 & constituency_no == 11 & poll_no == 1 & year == 2005 & position == 5 ~ 6,
	            state_name == "Goa" & assembly_no == 7 & constituency_no == 1 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Goa" & assembly_no == 7 & constituency_no == 5 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Goa" & assembly_no == 7 & constituency_no == 22 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Goa" & assembly_no == 7 & constituency_no == 11 & poll_no == 1 & year == 2019 ~ 5,
	            state_name == "Gujarat" & assembly_no == 13 & poll_no == 1 & year == 2018 ~ 12,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 21 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 64 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 77 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 85 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 8 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 16 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 20 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 32 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 50 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Gujarat" & assembly_no == 13 & constituency_no == 122 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Gujarat" & assembly_no == 13 & poll_no == 1 & year == 2020 ~ 11,
	            state_name == "Gujarat" & assembly_no == 13 & poll_no == 1 & year == 2021 ~ 4,
	            state_name == "Himachal Pradesh" & assembly_no == 13 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Himachal Pradesh" & assembly_no == 13 & poll_no == 1 & year == 2021 ~ 10,
	            state_name == "Karnataka" & assembly_no == 9 & is.na(month) & poll_no == 1 & year == 2016 ~ 2,
	            state_name == "Karnataka" & assembly_no == 10 & is.na(month) & poll_no == 1 & year == 2018 ~ 11,
	            state_name == "Karnataka" & assembly_no == 10 & (constituency_no == 42 | constituency_no == 70) & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Karnataka" & assembly_no == 10 & is.na(month) & poll_no == 1 & year == 2019 ~ 12,
	            state_name == "Karnataka" & assembly_no == 10 & poll_no == 1 & year == 2020 ~ 11,
	            state_name == "Karnataka" & assembly_no == 10 & constituency_no == 47 & poll_no == 1 & year == 2021 ~ 4,
	            state_name == "Karnataka" & assembly_no == 10 & constituency_no == 59 & poll_no == 1 & year == 2021 ~ 4,
	            state_name == "Karnataka" & assembly_no == 10 & constituency_no == 33 & poll_no == 1 & year == 2021 ~ 10,
	            state_name == "Karnataka" & assembly_no == 10 & constituency_no == 82 & poll_no == 1 & year == 2021 ~ 10,
	            state_name == "Manipur" & assembly_no == 12 & is.na(month) & poll_no == 1 & year == 2020 ~ 11,
	            state_name == "Meghalaya" & is.na(month) & poll_no == 1 & year == 2018 ~ 8,
	            state_name == "Meghalaya" & is.na(month) & constituency_no == 26 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Meghalaya" & is.na(month) & constituency_no == 48 & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Meghalaya" & is.na(month) & poll_no == 1 & year == 2021 ~ 10,
	            state_name == "Nagaland" & is.na(month) & poll_no == 1 & year == 2000 ~ 10,
	            state_name == "Nagaland" & is.na(month) & poll_no == 1 & year == 2017 ~ 7,
	            state_name == "Nagaland" & is.na(month) & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Nagaland" & is.na(month) & poll_no == 1 & year == 2020 ~ 11,
	            state_name == "Nagaland" & is.na(month) & constituency_no == 51 & poll_no == 1 & year == 2021 ~ 4,
	            state_name == "Nagaland" & is.na(month) & constituency_no == 58 & poll_no == 1 & year == 2021 ~ 10,
	            state_name == "Punjab" & is.na(month) & poll_no == 1 & year == 2000 ~ 2,
	            state_name == "Punjab" & is.na(month) & poll_no == 1 & year == 2018 ~ 5,
	            state_name == "Punjab" & is.na(month) & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Tripura" & is.na(month) & poll_no == 1 & year == 2016 ~ 11,
	            state_name == "Tripura" & is.na(month) & poll_no == 1 & year == 2019 ~ 9,
	            state_name == "Tripura" & is.na(month) & poll_no == 1 & year == 2022 ~ 6,
	            state_name == "Uttar Pradesh" & constituency_no == 306 & poll_no == 1 & year == 2001 ~ 2,
	            state_name == "Uttar Pradesh" & constituency_no == 399 & poll_no == 1 & year == 2001 ~ 2,
	            state_name == "Uttar Pradesh" & constituency_no == 414 & poll_no == 1 & year == 2001 ~ 2,
	            state_name == "Uttar Pradesh" & constituency_no == 141 & poll_no == 1 & year == 2001 ~ 4,
	            state_name == "Uttar Pradesh" & constituency_no == 367 & poll_no == 1 & year == 2001 ~ 4,
	            state_name == "Uttar Pradesh" & constituency_no == 25 & poll_no == 1 & year == 2004 ~ 1,
	            state_name == "Uttar Pradesh" & constituency_no == 286 & poll_no == 1 & year == 2004 ~ 4,
	            state_name == "Uttar Pradesh" & constituency_no == 312 & poll_no == 1 & year == 2004 ~ 4,
	            state_name == "Uttar Pradesh" & constituency_no == 67 & poll_no == 1 & year == 2004 ~ 4,
	            state_name == "Uttar Pradesh" & constituency_no == 250 & poll_no == 1 & year == 2004 ~ 4,
	            state_name == "Uttar Pradesh" & constituency_no == 105 & poll_no == 1 & year == 2004 ~ 4,
	            state_name == "Uttar Pradesh" & constituency_no == 69 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 120 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 124 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 144 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 220 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 235 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 252 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 261 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 356 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 359 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 372 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 394 & poll_no == 1 & year == 2004 ~ 10,
	            state_name == "Uttar Pradesh" & constituency_no == 340 & poll_no == 1 & year == 2013 ~ 2,
	            state_name == "Uttar Pradesh" & constituency_no == 258 & poll_no == 1 & year == 2013 ~ 6,
	            state_name == "Uttar Pradesh" & constituency_no == 5 & poll_no == 1 & year == 2016 ~ 2,
	            state_name == "Uttar Pradesh" & constituency_no == 14 & poll_no == 1 & year == 2016 ~ 2,
	            state_name == "Uttar Pradesh" & constituency_no == 274 & poll_no == 1 & year == 2016 ~ 2,
	            state_name == "Uttar Pradesh" & constituency_no == 30 & poll_no == 1 & year == 2016 ~ 5,
	            state_name == "Uttar Pradesh" & constituency_no == 376 & poll_no == 1 & year == 2016 ~ 5,
	            state_name == "Uttar Pradesh" & is.na(month) & poll_no == 1 & year == 2018 ~ 5,
	            state_name == "Uttar Pradesh" & is.na(month) & constituency_no == 138 & poll_no == 1 & year == 2019 ~ 4,
	            state_name == "Uttar Pradesh" & is.na(month) & constituency_no == 89 & poll_no == 1 & year == 2019 ~ 5,
	            state_name == "Uttar Pradesh" & is.na(month) & constituency_no == 228 & poll_no == 1 & year == 2019 ~ 9,
	            state_name == "Uttar Pradesh" & is.na(month) & poll_no == 1 & year == 2019 ~ 10,
	            state_name == "Uttar Pradesh" & is.na(month) & poll_no == 1 & year == 2020 ~ 11,
	            state_name == "Uttarakhand" & is.na(month) & poll_no == 1 & year == 2018 ~ 5,
	            state_name == "Uttarakhand" & is.na(month) & poll_no == 1 & year == 2019 ~ 11,
	            state_name == "Uttarakhand" & is.na(month) & poll_no == 1 & year == 2021 ~ 4,
	            TRUE ~ month
	        ),
			st_code2001 = st_code_ec,
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
	    )

	return(df)

}

# Event Study Models for Alternative Channels #################################

event_study_channels <- function(
	outcome, # Character for outcome
	df # Data frame
	) {

	# Setting-up locals #######################################################

	if (outcome == "administrative_visits") {
		outcome_text <- "administrative_visits"
		outcome_log <- "log_administrative_visits"
		outcome_footnote <- "visits from a block or district level officer"
		outcome_lag <- "lag_administrative_visits"
		outcome_abbreviation <- "administrative"
		outcome_title <- "Administrative Visits"
		panel <- "A"
	} else if (outcome == "smc_meetings") {
		outcome_text <- "smc_meetings"
		outcome_log <- "log_smc_meetings"
		outcome_footnote <- "SMC meetings"
		outcome_lag <- "lag_smc_meetings"
		outcome_abbreviation <- "smc"
		outcome_title <- "SMC Meetings"
		panel <- "B"
	} else {
		stop("Incorrectly specified outcome. Please specify either administrative_vists or smc_meetings")
	}

	# Calculate election year means ###########################################

	# Reference Category Mean:
	sink(
		file = here(
			paste0(
				"data/output/text/electionyearmean",
				outcome_abbreviation,
				".tex"
			)
		)
	)
	cat(
		round(
			mean(
				df_src[[outcome_text]][df_src$private == 0 & df_src$vs_year_2 == 1],
				na.rm = TRUE
			),
			3
	    )
	)
	sink(file = NULL)

	# Set-up formulas #########################################################

	rhs_base <- c(
		"vs_year_0", "vs_year_1", "vs_year_3", "vs_year_4", "rural"
	)

	formula_base <- as.formula(
		paste(
			outcome_text,
			paste(rhs_base, collapse = " + "),
			sep = " ~ "
		)
	)

	formula_lag <- as.formula(
		paste(
			outcome_text,
			paste(c(rhs_base, "lag_absent"), collapse = " + "),
			sep = " ~ "
		)
	)

	formula_teachers <- as.formula(
		paste(
			outcome_text,
			paste(c(rhs_base, "teachers"), collapse = " + "),
			sep = " ~ "
		)
	)

	formula_teachers_lag <- as.formula(
		paste(
			outcome_text,
			paste(c(rhs_base, "teachers", "lag_absent"), collapse = " + "),
			sep = " ~ "
		)
	)

	# Run models, no teacher controls #########################################

	# No Fixed Effects
	lm_no_fe <- feols(formula_lag,
	    cluster = c("unique_constituency_code", "year"),
	    data = subset(df, private == 0)
	)

	# Year fixed effects
	lm_year_fe <- feols(formula_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "year",
	    data = subset(df, private == 0)
	)

	# School fixed effects
	lm_school_fe <- feols(formula_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "school_code",
	    data = subset(df, private == 0)
	)

	n_school_fe <- format(
		lm_school_fe[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# School + Year Fixed Effects
	lm_year_school_fe <- feols(
		formula_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = subset(df, private == 0)
	)
	
	n_year_school_fe <- format(
		lm_year_school_fe[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# No lag
	lm_year_school_fe_no_lag <- feols(
		formula_base,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = subset(df, private == 0)
	)
	
	n_year_school_fe_no_lag <- format(
		lm_year_school_fe_no_lag[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# Dummy for absent, teacher controls ######################################

	# No Fixed Effects
	lm_no_fe_teachers <- feols(
		formula_teachers_lag,
	    cluster = c("unique_constituency_code", "year"),
	    data = subset(df, private == 0)
	)

	# Year fixed effects
	lm_year_fe_teachers <- feols(
		formula_teachers_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "year",
	    data = subset(df, private == 0)
	)

	# School fixed effects
	lm_school_fe_teachers <- feols(
		formula_teachers_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = "school_code",
	    data = subset(df, private == 0)
	)
	
	n_school_fe_teachers <- format(
		lm_school_fe_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# School + Year Fixed Effects
	lm_year_school_fe_teachers <- feols(
		formula_teachers_lag,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = subset(df, private == 0)
	)
	
	n_year_school_fe_teachers <- format(
		lm_year_school_fe_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# No lag
	lm_year_school_fe_no_lag_teachers <- feols(
		formula_teachers,
	    cluster = c("unique_constituency_code", "year"),
	    fixef = c("year", "school_code"),
	    data = subset(df, private == 0)
	)
	
	n_year_school_fe_no_lag_teachers <- format(
		lm_year_school_fe_no_lag_teachers[["fixef_sizes"]][["school_code"]],
	    big.mark = ",",
	    trim = TRUE
	)

	# Exporting models of absence dummy  ######################################

	models_list <- list(
		lm_no_fe, lm_year_fe, lm_school_fe, lm_year_school_fe,
		lm_year_school_fe_no_lag, lm_no_fe_teachers, lm_year_fe_teachers,
		lm_school_fe_teachers, lm_year_school_fe_teachers,
		lm_year_school_fe_no_lag_teachers
	)

	f <- function(x) format(round(x, 3), big.mark = ",")
	gof <- list(
	    list("raw" = "fe: year", "clean" = "Year FE", "fmt" = f),
	    list("raw" = "fe: school_code", "clean" = "School FE", "fmt" = f),
	    list("raw" = "nobs", "clean" = "Observations", "fmt" = f)
	)

	rows <- tribble(
	    ~term, ~nofe, ~yearfe, ~schoolfe, ~yearschoolfe, ~nolag, ~nofeteachers, ~yearfeteachers, ~schoolfeteachers, ~yearschoolfeteachers, ~nolagteachers,
	    "Year FE", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes",
	    "School FE", "No", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes",
	    "Lagged DV", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "No",
	    "Teacher Controls", "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes",
	    "Number of Schools", n_school_fe, n_school_fe, n_school_fe, n_year_school_fe, n_year_school_fe_no_lag, n_school_fe_teachers, n_school_fe_teachers, n_school_fe_teachers, n_year_school_fe_teachers, n_year_school_fe_no_lag_teachers
	)
	attr(rows, "position") <- c(9, 10, 11, 12, 13)

	coef_map <- c(
		"vs_year_0" = "2 or More Years Before Election",
        "vs_year_1" = "1 Year Before Election",
        "vs_year_3" = "1 Year After Election",
        "vs_year_4" = "2 or More Years After Election"
    )
    figure_reference <- paste0(
    	" Column 9 corresponds to Panel ",
    	panel,
    	" of Figure \\ref{fig:alternativechannels}."
    )

	table_note <- paste0(
		"\\emph{Notes}: * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01. Standard errors clustered at the constituency-year level in parentheses. The dependent variable is a dummy variable that takes the value of one if the school reports any ", outcome_footnote, " in that year. Each specification includes a dummy for whether the school is in a rural area. Columns 1-4 and 6-9 also include a lagged dependent variable. The election year mean is \\input{data/output/text/electionyearmean",
		outcome_abbreviation,
		".tex}.",
		figure_reference
	)

	any_absence_dise_table <- modelsummary(
		models_list,
	    title = paste0(
	    	"Any ",
	    	outcome_title,
	    	" Over the Electoral Cycle in Government Schools Using DISE Data",
	    	"\\label{",
	    	"table:",
	    	outcome_abbreviation,
	    	"}"
	    ),
	    coef_map = coef_map,
	    stars = c("*" = .1, "**" = .05, "***" = .01),
	    estimate = "{estimate}{stars}",
	    gof_map = gof,
	    add_rows = rows,
	    notes = table_note,
	    output = "tinytable",
	    escape = FALSE
	)

	if(outcome == "administrative_visits") {
		any_absence_dise_table %>%
			group_tt(j = list("Administrative Visits" = 2:11)) %>%
			style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
			save_tt(
				here("data/output/tables/administrative.tex"),
				overwrite = TRUE
			)
	}
	else {
		any_absence_dise_table %>%
			group_tt(j = list("SMC Meetings" = 2:11)) %>%
			style_tt(i = -1:nrow(.), fontsize = 0.7) %>%
			save_tt(here("data/output/tables/smc.tex"), overwrite = TRUE)
	}
	

	# Footer Material #########################################################

	# Sample sizes for figures

	sink(file = 
		here(
			paste0(
				"data/output/text/nschools",
				outcome_abbreviation,
				".tex"
			)
		)
	)
	cat(n_year_school_fe)
	sink(file = NULL)

	sink(file = 
		here(
			paste0(
				"data/output/text/nobs",
				outcome_abbreviation,
				".tex"
			)
		)
	)
	cat(format(lm_year_school_fe_teachers$nobs, big.mark = ","))
	sink(file = NULL)
}

# Summary Statistics Table ####################################################

SummaryStatisticsTable <- function(var) {
    mean_gov <- mean(df_src[[var]][df_src$private == 0], na.rm = TRUE)
    sd_gov <- sd(df_src[[var]][df_src$private == 0], na.rm = TRUE)
    n_gov <- length(df_src[[var]][df_src$private == 0])

    mean_private <- mean(df_src[[var]][df_src$private == 1], na.rm = TRUE)
    sd_private <- sd(df_src[[var]][df_src$private == 1], na.rm = TRUE)
    n_private <- length(df_src[[var]][df_src$private == 1])

    mean <- mean(df_src[[var]], na.rm = TRUE)
    sd <- sd(df_src[[var]], na.rm = TRUE)
    n <- length(df_src[[var]])

    model <- lm(df_src[[var]] ~ private, data = df_src)

    difference <- model$coefficients[["private"]]
    se_difference <- sqrt(diag(vcov(model)))[2]

    return_vector <- data.frame(
    	summary_statistics1 = mean_gov,
    	summary_statistics2 = sd_gov,
    	summary_statistics3 = n_gov,
    	summary_statistics4 = mean_private,
    	summary_statistics5 = sd_private,
    	summary_statistics6 = n_private,
    	summary_statistics7 = difference,
    	summary_statistics8 = se_difference,
    	summary_statistics9 = mean,
    	summary_statistics10 = sd,
    	summary_statistics11 = n
    )

    return(return_vector)
    
}