#Reading and cleaning datasets
all_transit <- read_csv("data/clean/treated_ntd.csv")
all_transit$uace_cd <- ifelse(all_transit$uza_name == "Livermore, CA", 50527, all_transit$uace_cd)
census <- read_csv("data/clean/census.csv") %>%
  filter(year >= 2014 & year <= 2019)
census$uza_name <- gsub("\\s*\\([^()]*\\)\\s*", "", census$uza_name)
census$uza_name <- gsub("\\s*urbanized\\s*area\\s*", "", census$uza_name, ignore.case = TRUE)

#Merging 2016-2019 data by geoid
full_data <- all_transit %>%
  inner_join(census, by = c("uace_cd" = "geoid", "year" = "year")) %>%
  select(-uza_name.y) %>%
  rename(uza_name = uza_name.x)

full_data$ridership_pop <- full_data$ridership/full_data$pop
full_data$agency_id <- as.numeric(factor(full_data$agency))
full_data$vehic_per_capita <- full_data$num_vehic_commute/full_data$pop
full_data$pop_100000 <- full_data$pop/100000
full_data$med_inc_10000 <- full_data$med_house_income/10000

#Missing a few UZAs but none from treatment. 26622 - 24990 = 1632 missing observations
write_csv(full_data,"data/clean/ntd_census.csv")
