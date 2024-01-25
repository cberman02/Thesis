rail_data <- read_csv("data/rail_data.csv")
census_data <- read_csv("data/all_uza.csv")

#Some cleaning
census_data$uza_name <- sub("([A-Z]{2}).*", "\\1", census_data$uza_name) # Limiting census uza names
rail_data$uza_name <- gsub("--\\w+\\b", "", rail_data$uza_name) #removing -- weirdness
census_data$uza_name <- gsub("--\\w+\\b", "", census_data$uza_name)
census_data$uza_name <- trimws(gsub("(?i)urban", "", census_data$uza_name)) #removing urban
rail_data$uza_name <- ifelse(rail_data$uza_name == "Miami Lauderdale, FL", "Miami, FL", rail_data$uza_name) #Not quite accurate but using Miami as proxy for all Miami Dade county
rail_data$uza_name <- ifelse(rail_data$uza_name == "Los Angeles Beach Ana, CA" | 
                               rail_data$uza_name == "Los Angeles Beach, CA", 
                               "Los Angeles, CA", rail_data$uza_name) #using LA as proxy for LA Beach
census_data$uza_name <- ifelse(census_data$uza_name == "Los Angeles Beach Ana, CA" | 
                                 census_data$uza_name == "Los Angeles Beach, CA", 
                             "Los Angeles, CA", census_data$uza_name)
census_data$uza_name <- ifelse(census_data$uza_name == "New York, NY", 
                               "New York City, NY", census_data$uza_name) #changing New York to NYC
census_data$uza_name <- ifelse(census_data$uza_name == "Salt Lake City Valley City, UT", 
                               "Salt Lake City, UT", census_data$uza_name) #Changing to salt lake city

#Merging data
full_data <- rail_data %>%
  left_join(census_data, by = c("uza_name" = "uza_name", "year" = "year"))

write_csv(full_data, "data/census_rail.csv")

#Code to see what uza did not merge
vars_to_check <- c("med_age", "pop","white","black","asian","hispanic","poverty","med_house_income","bach")
check_na <- full_data[rowSums(is.na(full_data[, vars_to_check])) == length(vars_to_check), ]
print(unique(check_na$uza_name))
#Currently Johnstown, PA
#          Galveston City, TX
#          Morgantown, WV
# Galveston is not in census data, Johnstown and Morgantown are only missing recent data
