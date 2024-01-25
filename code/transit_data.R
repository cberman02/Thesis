rail_data <- read_excel(paste0("data/",'transit_data.xlsx'), sheet = "UPT") %>%
  clean_names() %>%
  filter(x3_mode == "Rail")


#Cleaning cols to represent date better
exclude_columns <- 1:10 # Specify the columns to exclude
extra_info <- rail_data[exclude_columns]
years <- sub(".*_(\\d+)$", "\\1", colnames(rail_data)[-exclude_columns]) #Extracting years
months <- sub("x(\\d+)_.*", "\\1", colnames(rail_data)[-exclude_columns]) #Extracting months
new_columns <- paste0(tolower(month.abb[as.integer(months)]), "_", years) # Create new column names with the desired format
colnames(rail_data)[-exclude_columns] <- new_columns # Rename columns

rail_data <- rail_data %>%
  group_by(agency) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop')

#Converting wide to long
rail_data <- rail_data %>% 
  pivot_longer(
    cols = `jan_2002`:`nov_2023`, 
    names_to = "date",
    values_to = "ridership"
  )

rail_data <- rail_data %>% 
  merge(extra_info, by = "agency")

#Creating month and year columns
rail_data$month <- rail_data$date %>% 
  str_extract("^[a-z]+")
rail_data$year <- rail_data$date %>% 
  str_extract("\\d+$") %>% 
  as.numeric()
#Converting to date object
rail_data$date <- as.Date(paste(rail_data$month, rail_data$year, "01", sep = "_"), format = "%b_%Y_%d") 

rail_data <- rail_data %>%
  filter(year >= 2005 & year < 2020) #Restricting to 2005-2019


write.csv(rail_data, "data/rail_data.csv", row.names = FALSE) #Saving full data

