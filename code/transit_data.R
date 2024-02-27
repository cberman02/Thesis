transit_data <- read_excel(paste0("data/raw/",'transit_data.xlsx'), sheet = "UPT") %>%
  clean_names()

#Cleaning cols to represent date better
exclude_columns <- 1:10 # Specify the columns to exclude
extra_info <- transit_data[exclude_columns]
years <- sub(".*_(\\d+)$", "\\1", colnames(transit_data)[-exclude_columns]) #Extracting years
months <- sub("x(\\d+)_.*", "\\1", colnames(transit_data)[-exclude_columns]) #Extracting months
new_columns <- paste0(tolower(month.abb[as.integer(months)]), "_", years) # Create new column names with the desired format
colnames(transit_data)[-exclude_columns] <- new_columns # Rename columns

transit_data <- transit_data %>%
  group_by(agency) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop')

#Converting wide to long
transit_data <- transit_data %>% 
  pivot_longer(
    cols = `jan_2002`:`nov_2023`, 
    names_to = "date",
    values_to = "ridership"
  )

transit_data <- transit_data %>% 
  merge(extra_info, by = "agency")

#Creating month and year columns
transit_data$month <- transit_data$date %>% 
  str_extract("^[a-z]+")
transit_data$year <- transit_data$date %>% 
  str_extract("\\d+$") %>% 
  as.numeric()
#Converting to date object
transit_data$date <- as.Date(paste(transit_data$month, transit_data$year, "01", sep = "_"), format = "%b_%Y_%d") 

transit_data <- transit_data %>%
  filter(year >= 2005 & year < 2020) #Restricting to 2005-2019

#Cleaning UZA names for easier merging
transit_data$uza_name <- gsub("--\\w+\\b", "", transit_data$uza_name) #removing -- weirdness
transit_data$uza_name <- gsub("City City", "City", transit_data$uza_name) #removing duplicate "city"
transit_data$uza_name <- ifelse(transit_data$uza_name == "Miami Lauderdale, FL", "Miami, FL", transit_data$uza_name) #Not quite accurate but using Miami as proxy for all Miami Dade county
transit_data$uza_name <- ifelse(transit_data$uza_name == "Los Angeles Beach Ana, CA" | 
                                  transit_data$uza_name == "Los Angeles Beach, CA", 
                                "Los Angeles, CA", transit_data$uza_name) #using LA as proxy for LA Beach


#819 unique agencies, over 15 years with ridership recorded monthly. 413,000 total observations
write.csv(transit_data, "data/transit_data.csv", row.names = FALSE) #Saving full data

