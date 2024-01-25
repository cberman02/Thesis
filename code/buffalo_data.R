transit <- read_excel(paste0("data/",'transit_data.xlsx'), sheet = "UPT") %>%
  clean_names()
rail <- transit %>%
  filter(x3_mode == "Rail")
buff_clev <- rail %>%
  filter(uza_name == "Buffalo, NY" | uza_name == "Cleveland, OH" | (uza_name == "Pittsburgh, PA" & mode != "IP"))

#Cleaning cols to represent date better
exclude_columns <- 1:10 # Specify the columns to exclude
years <- sub(".*_(\\d+)$", "\\1", colnames(buff_clev)[-exclude_columns]) #Extracting years
months <- sub("x(\\d+)_.*", "\\1", colnames(buff_clev)[-exclude_columns]) #Extracting months
new_columns <- paste0(tolower(month.abb[as.integer(months)]), "_", years) # Create new column names with the desired format
colnames(buff_clev)[-exclude_columns] <- new_columns # Rename columns

buff_clev_simple <- buff_clev %>%
  group_by(agency) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop')

buff_clev_simple <- buff_clev_simple %>% 
  pivot_longer(
    cols = `jan_2002`:`nov_2023`, 
    names_to = "date",
    values_to = "ridership"
  )

#Creating month and year columns
buff_clev_simple$month <- buff_clev_simple$date %>% 
  str_extract("^[a-z]+")
buff_clev_simple$year <- buff_clev_simple$date %>% 
  str_extract("\\d+$") %>% 
  as.numeric()
#Converting to date object
buff_clev_simple$date <- as.Date(paste(buff_clev_simple$month, buff_clev_simple$year, "01", sep = "_"), format = "%b_%Y_%d") 



buff_clev_simple <- buff_clev_simple %>% 
  filter(year < 2020) #Getting rid of covid years
# Create the line plot
ggplot(buff_clev_simple, aes(x = date, y = log(ridership), group = agency, color = as.factor(agency))) +
  geom_line() +
  labs(title = "Ridership Over Time",
       x = "Date",
       y = "Ridership",
       color = "Agency") +
  scale_y_continuous(labels = scales::number_format()) +
  geom_vline(xintercept = as.Date("2014-04-01"), color = "red", linetype = "dashed") + # Add a red dashed vertical line
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Rotate x-axis labels for better visibility


# Creating interaction terms for DiD
buff_clev_simple <- buff_clev_simple %>% 
  mutate(uber_legal = ifelse(agency == "The Greater Cleveland Regional Transit Authority", 1, 0))
buff_clev_simple <- buff_clev_simple %>% 
  mutate(post_2014 = ifelse(date >= as.Date("2014-04-01"), 1, 0))
buff_clev_simple <- buff_clev_simple %>%
  mutate(treated = uber_legal * post_2014)
write.csv(buff_clev_simple, "data/buff_clev_simple.csv", row.names = FALSE) #Saving full data

# Run the difference-in-differences regression
model <- lm(ridership ~ post_2014 + uber_legal + uber_legal * post_2014, data = buff_clev_simple)

# Display the results
summary(model)


