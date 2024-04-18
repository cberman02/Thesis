transit_upt <- read_excel(paste0("data/raw/",'transit_data.xlsx'), sheet = "UPT") %>%
  clean_names()

#Cleaning cols to represent date better for UPT
exclude_columns <- 1:10 # Specify the columns to exclude
extra_info <- transit_upt[exclude_columns]
years <- sub(".*_(\\d+)$", "\\1", colnames(transit_upt)[-exclude_columns]) #Extracting years
months <- sub("x(\\d+)_.*", "\\1", colnames(transit_upt)[-exclude_columns]) #Extracting months
new_columns <- paste0(tolower(month.abb[as.integer(months)]), "_", years) # Create new column names with the desired format
colnames(transit_upt)[-exclude_columns] <- new_columns # Rename columns

transit_upt <- transit_upt %>%
  group_by(agency) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop')

#Converting wide to long
transit_upt <- transit_upt %>% 
  pivot_longer(
    cols = `jan_2002`:`nov_2023`, 
    names_to = "date",
    values_to = "ridership"
  )

transit_upt <- transit_upt %>% 
  merge(extra_info, by = "agency")

#Creating month and year columns
transit_upt$month <- transit_upt$date %>% 
  str_extract("^[a-z]+")
transit_upt$year <- transit_upt$date %>% 
  str_extract("\\d+$") %>% 
  as.numeric()
#Converting to date object
transit_upt$date <- as.Date(paste(transit_upt$month, transit_upt$year, "01", sep = "_"), format = "%b_%Y_%d") 

transit_upt <- transit_upt %>%
  filter(status == "Active") #Restricted to 2016 and on
transit_upt$ridership[transit_upt$ridership == 0] <- 1 #Making 0 to 1 for ln calculation
  
#Cleaning UZA names for easier merging
transit_upt$uza_name <- gsub("--\\w+\\b", "", transit_upt$uza_name) #removing -- weirdness
transit_upt$uza_name <- gsub("City City", "City", transit_upt$uza_name) #removing duplicate "city"
transit_upt$uza_name <- ifelse(transit_upt$uza_name == "Miami Lauderdale, FL", "Miami, FL", transit_upt$uza_name) #Not quite accurate but using Miami as proxy for all Miami Dade county
transit_upt$uza_name <- ifelse(transit_upt$uza_name == "Los Angeles Beach Ana, CA" | 
                                  transit_upt$uza_name == "Los Angeles Beach, CA", 
                                "Los Angeles, CA", transit_upt$uza_name) #using LA as proxy for LA Beach

transit_upt <- transit_upt %>%
  separate(col = uza_name, into = c("city", "state"), sep = ", ", remove = FALSE)

#Adding in total UZA ridership
uza_ridership <- transit_upt %>%
  group_by(uza_name, date) %>%
  summarise(uza_ridership = sum(ridership))

state_ridership <- transit_upt %>%
  group_by(state, date) %>%
  summarise(state_ridership = sum(ridership))

transit_upt <- merge(transit_upt, uza_ridership, by = c("uza_name", "date"))
transit_upt <- merge(transit_upt, state_ridership, by = c("state", "date"))

write.csv(transit_upt, "data/clean/upt_data.csv", row.names = FALSE) #Saving full data





