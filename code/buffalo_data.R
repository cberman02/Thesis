rail <- read.csv("data/rail_data.csv") #Reading in data

buff_clev <- rail %>%
  filter(uza_name == "Buffalo, NY" | uza_name == "Cleveland, OH" | (uza_name == "Pittsburgh, PA" & mode != "IP"))

# Create the line plot
ggplot(buff_clev, aes(x = date, y = log(ridership), group = agency, color = as.factor(agency))) +
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
buff_clev <- buff_clev %>% 
  mutate(uber_legal = ifelse(agency == "The Greater Cleveland Regional Transit Authority", 1, 0))
buff_clev <- buff_clev %>% 
  mutate(post_2014 = ifelse(date >= as.Date("2014-04-01"), 1, 0))
buff_clev <- buff_clev %>%
  mutate(treated = uber_legal * post_2014)
write.csv(buff_clev, "data/buff_clev.csv", row.names = FALSE) #Saving full data

# Run the difference-in-differences regression
model <- lm(ridership ~ post_2014 + uber_legal + uber_legal * post_2014, data = buff_clev_simple)

# Display the results
summary(model)


