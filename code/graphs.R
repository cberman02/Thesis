transit_data <- read.csv("data/transit_data.csv")
pre_covid <- read.csv("data/pre_covid.csv")
with_covid <- read.csv("data/with_covid.csv")
transit_rankings <- read.csv("data/transit_rankings.csv")
transit_data$date <- as.Date(transit_data$date)
pre_covid$date <- as.Date(pre_covid$date)
with_covid$date <- as.Date(with_covid$date)

#Avg rides per month
avg_rides <-transit_data %>%
  group_by(agency) %>%
  summarise(ridership = mean(ridership))

avg_rides_plot <- ggplot(avg_rides,aes(x = ridership)) +
  geom_histogram(binwidth = 200000, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Average Ridership per Month",
       x = "Average Ridership") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

#Removing NYC
avg_rides <- avg_rides %>%
  filter(agency != "MTA New York City Transit")

avg_rides_no_nyc <- ggplot(avg_rides,aes(x = ridership)) +
  geom_histogram(binwidth = 200000, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Average Ridership per Month", 
       x = "Average Ridership",
       caption = "Without MTA New York City Transit") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

g <- arrangeGrob(avg_rides_plot, avg_rides_no_nyc, nrow=2) #generates g
ggsave(file="images/avg_rides_per_agency.png", g) #saves g


# Group by UZA and count the number of unique transit agencies
uza_agency_counts <- transit_data %>%
  group_by(uza_name) %>%
  summarise(unique_agencies = n_distinct(agency))

# Create a histogram of the counts
ggplot(uza_agency_counts, aes(x = unique_agencies)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Transit Agencies per UZA",
       x = "Number of Transit Agencies",
       y = "Frequency")
ggsave("images/avg_agencies_uza.png")

# COEFFICIENT PLOT
#Running log(ridership) + treated*time
pre_covid_abbr <- pre_covid %>%
  filter(date >= "2016-01-01") 
did_uza_ln <- feols(log(ridership) ~ i(date, treated, "2019-12-01") |+ ## Our key interaction: time × treatment status
                    agency + date,                             ## FEs
                    cluster = ~agency,                          ## Clustered SEs
                    data = pre_covid_abbr)

pre_covid_abbr <- pre_covid_abbr %>%
  filter(date != "2019-12-01") #filtering out date to prevent colinearity

graph_df <- data.frame(date = unique(pre_covid_abbr$date),
                       coef = did_uza_ln$coefficients,
                       ci = did_uza_ln$se*1.96) %>%
  mutate(date = as.Date(date)) #Creating df to make ggplot graph from
ggplot(graph_df, aes(x = date, y = coef)) +
  geom_point() +
  geom_errorbar(aes(ymin = coef-ci, ymax = coef+ci), width = 0.2) +
  geom_vline(xintercept = as.Date("2019-12-01"), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid", color = "blue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Abbreviate x-axis labels to display only the year
  labs(x = "Year", y = "Coefficient", title = "Coefficient Plot")
ggsave("images/coeff_plot.png")

#Limiting to 3 months before and 3 months after
ggplot(graph_df, aes(x = date, y = coef)) +
  geom_point() +
  geom_errorbar(aes(ymin = coef-ci, ymax = coef+ci), width = 0.2) +
  xlim(as.Date("2019-01-01"), as.Date("2020-03-01")) +
  geom_vline(xintercept = as.Date("2019-12-01"), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid", color = "blue") +
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Abbreviate x-axis labels to display only the year
  labs(x = "Year", y = "Coefficient", title = "Coefficient Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("images/coeff_plot_1year.png")



#Ridership month by month
average_ridership <- transit_data %>%
  group_by(date) %>%
  summarise(average_ridership = mean(ridership),log_ridership = mean(log(ridership)))

# Create the line plot
rides_month <- ggplot(data = average_ridership, aes(x = date, y = average_ridership)) +
  geom_line(color = "blue") +
  #geom_point(color = "blue") +  # Add points for each data point
  labs(title = "Average Ridership Over Time",
       x = "Time",
       y = "Average Ridership") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y")  # Display dates at yearly intervals

log_rides <- ggplot(data = average_ridership, aes(x = date, y = log_ridership)) +
  geom_line(color = "blue") +
  #geom_point(color = "blue") +  # Add points for each data point
  labs(title = "Log Average Ridership Over Time",
       x = "Time",
       y = "Average Ridership") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y")  # Display dates at yearly intervals

b <- arrangeGrob(rides_month, log_rides, nrow=2) #generates g
ggsave(file="images/avg_ridership_over_time.png", b) #saves g

#Transit rankings histos
ggplot(transit_rankings,aes(x = score)) +
  geom_histogram(binwidth = 0.25,fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Average Agency Score") +
  geom_vline(xintercept = mean(transit_rankings$score), color = "red") +
  geom_vline(xintercept = quantile(transit_rankings$score, c(0.125, 0.875), na.rm = TRUE), color = "red") +
  theme_minimal() 
ggsave("images/avg_agency_score.png")

#Ridership month over month with separated treatment and controls
avg_ridership_control <- pre_covid %>%
  filter(treated == 0) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log(ridership)))
avg_ridership_treated <- pre_covid %>%
  filter(treated == 1) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log(ridership)))

ggplot() +
  geom_line(data = avg_ridership_treated, aes(x = date, y = log_ridership, color = "Treated")) +
  geom_line(data = avg_ridership_control, aes(x = date, y = log_ridership, color = "Untreated")) +
  labs(title = "Average Ridership Over Time",
       x = "Date",
       y = "Average Ridership",
       color = "Treatment Status") +
  theme_minimal()
ggsave("images/avg_ridership_over_time_treated.png")

#Establishing placebo parallel
placebo <- pre_covid
placebo$treated <- ifelse(placebo$agency %in%
                              c("Pinellas Suncoast Transit Authority",
                                "Livermore / Amador Valley Transit Authority",
                                "Research Triangle Regional Public Transportation Authority"), 0, 0)
placebo$treated <- ifelse(placebo$agency %in%
                              c("Hillsborough Area Regional Transit Authority",
                                "The Eastern Contra Costa Transit Authority",
                                "Town of Chapel Hill"), 1, 0)
avg_ridership_treated <- pre_covid %>%
  filter(treated == 1) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log(ridership)))
avg_ridership_placebo <- placebo %>%
  filter(treated == 1) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log(ridership)))

ggplot() +
  geom_line(data = avg_ridership_treated, aes(x = date, y = log_ridership, color = "Real Treated")) +
  geom_line(data = avg_ridership_placebo, aes(x = date, y = log_ridership, color = "Placebo Treated")) +
  #geom_vline(xintercept = as.numeric(as.Date("2019-12-01")), linetype = "dashed", color = "red") +
  labs(title = "Average Ridership Over Time",
       x = "Date",
       y = "Average Ridership",
       color = "Treatment Status") +
  theme_minimal()
ggsave("images/avg_ridership_over_time_placebo.png")
