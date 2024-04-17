#Reading and cleaning datasets
all_transit <- read_csv("data/transit_treated_new_start.csv")
all_transit$uace_cd <- ifelse(all_transit$uza_name == "Livermore, CA", 50527, all_transit$uace_cd)
census <- read_csv("data/clean/all_uza.csv") %>%
  filter(year >= 2016 & year <= 2019)
census$uza_name <- gsub("\\s*\\([^()]*\\)\\s*", "", census$uza_name)
census$uza_name <- gsub("\\s*urbanized\\s*area\\s*", "", census$uza_name, ignore.case = TRUE)

#Merging 2016-2019 data by geoid
full_data <- all_transit %>%
  filter(year != 2020) %>%
  inner_join(census, by = c("uace_cd" = "geoid", "year" = "year")) %>%
  select(-uza_name.y) %>%
  rename(uza_name = uza_name.x)

#Repeating 2019 ACS data for 2020 variables and merging with full_data
census_2020 <- census %>%
  filter(year == 2019) %>%
  mutate(year=2020)
full_data_2020 <- all_transit %>%
  filter(year == 2020) %>%
  inner_join(census_2020, by = c("uace_cd" = "geoid", "year" = "year"))%>%
  select(-uza_name.y) %>%
  rename(uza_name = uza_name.x)
full_data <- full_data %>%
  rbind(full_data_2020)

#Missing a few UZAs but none from treatment. 26622 - 24990 = 1632 missing observations
write_csv(full_data,"data/clean/all_transit_census.csv")
