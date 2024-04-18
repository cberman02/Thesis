transit_data <- read_csv("data/transit_data.csv") %>%
  filter(year >= 2005)
census_data <- read_csv("data/clean/all_uza.csv") 
census_data$uza_name <- gsub("\\s*\\([^()]*\\)\\s*", "", census_data$uza_name)
census_data$uza_name <- gsub("\\s*urbanized\\s*area\\s*", "", census_data$uza_name, ignore.case = TRUE)

transit_rankings <- read_csv("data/transit_rankings.csv")

#Merging data. Changed left -> inner. 338 different cities all with matching census data (excluding 2020)
full_data <- transit_data %>%
  filter(year != 2020) %>%
  inner_join(census_data, by = c("uace_cd" = "geoid", "year" = "year"))

#Repeating 2019 ACS data for 2020 variables
acs_2020 <- census_data %>%
  filter(year == 2019) %>%
  mutate(year=2020)
full_data_2020 <- transit_data %>%
  filter(year == 2020) %>%
  inner_join(acs_2020, by = c("uace_cd" = "geoid", "year" = "year"))

full_data <- full_data %>%
  rbind(full_data_2020)

# Perform an anti-join to get the unmatched rows from the original merge
unmatched_rows <- anti_join(transit_data,census_data, by = c("uace_cd" = "geoid", "year" = "year"))

# Next, try merging the unmatched rows with census_data by UZA name
full_data_uza <- unmatched_rows %>%
  inner_join(census_data, by = c("uza_name" = "uza_name","year" = "year"))

#Merging 2020 ACS data with unmatched rows by uza_name
full_data_uza_2020 <- unmatched_rows %>%
  filter(year == 2020) %>%
  inner_join(acs_2020, by = c("uza_name" = "uza_name", "year" = "year"))

full_data_uza <- full_data_uza %>%
  rbind(full_data_uza_2020) %>%
  select(-geoid)

full_data <- full_data %>%
  select(-uza_name.y) %>%
  rename(uza_name = uza_name.x) %>%
  rbind(full_data_uza)

full_data$uza_name_2 = full_data$uza_name
full_data <- separate(full_data, col = uza_name_2, into = c("city", "state"), sep = ", ", remove = FALSE) %>%
  select(-uza_name_2)



write_csv(full_data, "data/transit_census.csv")

full_data <- left_join(full_data, transit_rankings, by = "uza_name")

#Checking to see if any transit rankings are missing
#missing_elements <- transit_rankings$uza_name[!transit_rankings$uza_name %in% full_data$uza_name]


write_csv(full_data, "data/transit_census_rankings.csv")

                              