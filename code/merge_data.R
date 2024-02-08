transit_data <- read_csv("data/transit_data.csv")
census_data <- read_csv("data/all_uza.csv")

#Consider using CPS

#Some cleaning
census_data$uza_name <- sub("([A-Z]{2}).*", "\\1", census_data$uza_name) # Limiting census uza names
transit_data$uza_name <- gsub("--\\w+\\b", "", transit_data$uza_name) #removing -- weirdness
census_data$uza_name <- gsub("--\\w+\\b", "", census_data$uza_name)
transit_data$uza_name <- gsub("City City", "City", transit_data$uza_name) #removing duplicate "city"
census_data$uza_name <- trimws(gsub("(?i)urban", "", census_data$uza_name)) #removing urban
transit_data$uza_name <- ifelse(transit_data$uza_name == "Miami Lauderdale, FL", "Miami, FL", transit_data$uza_name) #Not quite accurate but using Miami as proxy for all Miami Dade county
transit_data$uza_name <- ifelse(transit_data$uza_name == "Los Angeles Beach Ana, CA" | 
                               transit_data$uza_name == "Los Angeles Beach, CA", 
                               "Los Angeles, CA", transit_data$uza_name) #using LA as proxy for LA Beach
census_data$uza_name <- ifelse(census_data$uza_name == "Los Angeles Beach Ana, CA" | 
                                 census_data$uza_name == "Los Angeles Beach, CA", 
                             "Los Angeles, CA", census_data$uza_name)
census_data$uza_name <- ifelse(census_data$uza_name == "New York, NY", 
                               "New York City, NY", census_data$uza_name) #changing New York to NYC
census_data$uza_name <- ifelse(census_data$uza_name == "Salt Lake City Valley City, UT", 
                               "Salt Lake City, UT", census_data$uza_name) #Changing to salt lake city

#Merging data. Changed left -> inner. 338 different cities all with matching census data
full_data <- transit_data %>%
  inner_join(census_data, by = c("uza_name" = "uza_name", "year" = "year")) %>%
  filter(ridership != 0)

full_data <- separate(full_data, col = uza_name, into = c("city", "state"), sep = ", ", remove = FALSE)


write_csv(full_data, "data/census_transit.csv")

#Code to see what uza did not merge
vars_to_check <- c("med_age", "pop","white","black","asian","hispanic","poverty","med_house_income","bach")
check_na <- full_data[rowSums(is.na(full_data[, vars_to_check])) == length(vars_to_check), ]
print(unique(check_na$uza_name))
#Needs further cleaning

                              