transit_vrh <- read_excel(paste0("data/raw/",'transit_data.xlsx'), sheet = "VRH") %>%
  clean_names()

#Cleaning cols to represent date better for UPT
exclude_columns <- 1:9 # Specify the columns to exclude
extra_info <- transit_vrh[exclude_columns]
years <- sub(".*_(\\d+)$", "\\1", colnames(transit_vrh)[-exclude_columns]) #Extracting years
months <- sub("x(\\d+)_.*", "\\1", colnames(transit_vrh)[-exclude_columns]) #Extracting months
new_columns <- paste0(tolower(month.abb[as.integer(months)]), "_", years) # Create new column names with the desired format
colnames(transit_vrh)[-exclude_columns] <- new_columns # Rename columns

transit_vrh <- transit_vrh %>%
  group_by(agency) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop')

#Converting wide to long
transit_vrh <- transit_vrh %>% 
  pivot_longer(
    cols = `jan_2002`:`nov_2023`, 
    names_to = "date",
    values_to = "vrh"
  )

transit_vrh <- transit_vrh %>% 
  merge(extra_info, by = "agency")

#Creating month and year columns
transit_vrh$month <- transit_vrh$date %>% 
  str_extract("^[a-z]+")
transit_vrh$year <- transit_vrh$date %>% 
  str_extract("\\d+$") %>% 
  as.numeric()
#Converting to date object
transit_vrh$date <- as.Date(paste(transit_vrh$month, transit_vrh$year, "01", sep = "_"), format = "%b_%Y_%d") 

transit_vrh <- transit_vrh %>%
  filter(status == "Active") #Restricted to 2016 and on

#Cleaning UZA names for easier merging
transit_vrh$uza_name <- gsub("--\\w+\\b", "", transit_vrh$uza_name) #removing -- weirdness
transit_vrh$uza_name <- gsub("City City", "City", transit_vrh$uza_name) #removing duplicate "city"
transit_vrh$uza_name <- ifelse(transit_vrh$uza_name == "Miami Lauderdale, FL", "Miami, FL", transit_vrh$uza_name) #Not quite accurate but using Miami as proxy for all Miami Dade county
transit_vrh$uza_name <- ifelse(transit_vrh$uza_name == "Los Angeles Beach Ana, CA" | 
                                  transit_vrh$uza_name == "Los Angeles Beach, CA", 
                                "Los Angeles, CA", transit_vrh$uza_name) #using LA as proxy for LA Beach

transit_vrh <- transit_vrh %>%
  separate(col = uza_name, into = c("city", "state"), sep = ", ", remove = FALSE)

write.csv(transit_vrh, "data/clean/vrh_data.csv", row.names = FALSE) #Saving full data
