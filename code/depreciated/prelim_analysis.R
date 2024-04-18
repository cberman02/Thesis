transit_data <- read.csv("data/transit_data.csv") %>%
  separate(col = uza_name, into = c("city", "state"), sep = ", ", remove = FALSE)
transit_census <- read.csv("data/transit_census.csv")
transit_census_rankings <- read.csv("data/transit_census_rankings.csv")

reg_pre_covid <- transit_census_rankings 
reg_covid <- transit_census_rankings %>%
  filter(year != 2020) 

#Trying just transit data to see why things changed
reg_pre_covid_transit <- transit_data
reg_pre_covid_transit$treated <- ifelse(reg_pre_covid_transit$agency %in%
                                  c("Pinellas Suncoast Transit Authority",
                                    "Livermore / Amador Valley Transit Authority",
                                    "Research Triangle Regional Public Transportation Authority"), 1, 0)
reg_pre_covid_transit <- reg_pre_covid_transit %>%
  filter(date <= "2020-03-01") #Restricted to before COVID
reg_pre_covid_transit$time <- ifelse(reg_pre_covid_transit$date > "2019-12-01", 1, 0)
write_csv(reg_pre_covid_transit, "data/pre_covid_transit.csv")

did_fe_date <- feols(ridership ~ treated:time | agency + date, data = reg_pre_covid_transit)
did_ln_date <- feols(log(ridership) ~ treated:time | agency + date, data = reg_pre_covid_transit)
etable(did_fe_date, did_ln_date, tex = T)



#For simple regression (all below agencies are 2020 DiD)
reg_covid$treated <- ifelse(reg_covid$agency %in% 
                                  c("Dallas Area Rapid Transit", 
                                  "Bi-State Development Agency of the Missouri-Illinois Metropolitan District",
                                  "Pinellas Suncoast Transit Authority",
                                  "Livermore / Amador Valley Transit Authority",
                                  "Research Triangle Regional Public Transportation Authority"), 1, 0) 
#Livermore and RTP 12/2019, PSTA 11/2019, just treating as 2020

reg_covid$time <- ifelse(reg_covid$year > 2020, 1, 0) 

reg_pre_covid$treated <- ifelse(reg_pre_covid$agency %in%
                                  c("Pinellas Suncoast Transit Authority",
                                  "Livermore / Amador Valley Transit Authority",
                                  "Research Triangle Regional Public Transportation Authority"), 1, 0)
reg_pre_covid <- reg_pre_covid %>%
  filter(date <= "2020-03-01") #Restricted to before COVID
reg_pre_covid$time <- ifelse(reg_pre_covid$date > "2019-12-01", 1, 0)

write_csv(reg_pre_covid, "data/pre_covid.csv")
write_csv(reg_covid, "data/with_covid.csv")

# DiD regression with fixed effects using feols using pre covid
did_base <- feols(ridership ~ treated:time, data = reg_pre_covid)
did_fe <- feols(ridership ~ treated:time | agency + month, data = reg_pre_covid)
did_ln <- feols(log(ridership) ~ treated:time | agency + month, data = reg_pre_covid)
did_fe_date <- feols(ridership ~ treated:time | agency + date, data = reg_pre_covid)
did_ln_date <- feols(log(ridership) ~ treated:time | agency + date, data = reg_pre_covid)

#W/ Covid
did_base_cov <- feols(ridership ~ treated:time, data = reg_covid)
did_fe_cov <- feols(ridership ~ treated:time | agency + month, data = reg_covid)
did_ln_cov <- feols(log(ridership) ~ treated:time | agency + month, data = reg_covid)
did_fe_year_cov <- feols(ridership ~ treated:time | agency + date, data = reg_covid)
did_ln_year_cov <- feols(log(ridership) ~ treated:time | agency + date, data = reg_covid)

pre_covid <- etable(did_base, did_fe, did_ln, did_fe_date, did_ln_date, tex = T)
during_covid <- etable(did_base_cov, did_fe_cov, did_ln_cov, did_fe_year_cov, did_ln_year_cov,tex = T)

#Mode fixed effects
#did_mode_fe <- feols(ridership ~ treated:time | agency + x3_mode + month, data = reg_pre_covid)
#did_mode_ln <- feols(log(ridership) ~ treated:time | agency + x3_mode + month, data = reg_pre_covid)

#etable(did_mode_fe, did_mode_ln)

#Trying UZA ridership, not agency ridership
did_uza <- feols(total_ridership ~ treated:time | agency + date, data = reg_pre_covid)
did_uza_ln <- feols(log(total_ridership) ~ treated:time | agency + date, data = reg_pre_covid)
etable(did_fe_date, did_ln_date,did_uza, did_uza_ln, tex = T)


#Using ranking controls for pre-covid (ACS for 2020 is just 2019 repeated)
did_rank_fe <- feols(ridership ~ treated:time| agency + date + score, data = reg_pre_covid)
did_rank_ln <- feols(log(ridership) ~ treated:time| agency + date + score, data = reg_pre_covid)
etable(did_rank_fe, did_rank_ln)

#Pop controls
did_pop_fe <- feols(total_ridership ~ treated:time| agency + date + pop, data = reg_pre_covid)
did_pop_ln <- feols(log(total_ridership) ~ treated:time| agency + date + pop, data = reg_pre_covid)
did_pop_1 <- feols(log(total_ridership) ~ treated:time| agency + date + med_age, data = reg_pre_covid)
did_pop_2 <- feols(log(total_ridership) ~ treated:time| agency + date + white, data = reg_pre_covid)
did_pop_3 <- feols(log(total_ridership) ~ treated:time| agency + date + med_house_income, data = reg_pre_covid)
etable(did_pop_fe, did_pop_ln, did_pop_1, did_pop_2, did_pop_3, tex = T)

did_pop_fe <- feols(ridership ~ treated:time| agency + date + pop, data = reg_pre_covid)
did_pop_ln <- feols(log(ridership) ~ treated:time| agency + date + pop, data = reg_pre_covid)
did_pop_1 <- feols(log(ridership) ~ treated:time| agency + date + med_age, data = reg_pre_covid)
did_pop_2 <- feols(log(ridership) ~ treated:time| agency + date + white, data = reg_pre_covid)
did_pop_3 <- feols(log(ridership) ~ treated:time| agency + date + med_house_income, data = reg_pre_covid)
etable(did_pop_fe, did_pop_ln, did_pop_1, did_pop_2, did_pop_3, tex = T)
