transit_data <- read.csv("data/transit_data.csv")
pre_covid <- read.csv("data/pre_covid.csv")
with_covid <- read.csv("data/with_covid.csv")
transit_rankings <- read.csv("data/transit_rankings.csv")
transit_data$date <- as.Date(transit_data$date)

avg_rides <-transit_data %>%
  group_by(agency) %>%
  summarise(ridership = mean(ridership))

#Avg rides per month
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
#iplot(did_uza_ln, 
      #xlab = 'Time to treatment',
      #main = 'Coefficient Plot') 
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Abbreviate x-axis labels to display only the year
  labs(x = "Year", y = "Coefficient", title = "Coefficient Plot")

# Plot
ggplot(t_test_df, aes(x = factor(year))) +
  geom_point(aes(y = diff), color = "blue") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a line at y = 0
  geom_vline(xintercept = 2019, linetype = "dashed", color = "green") +
  labs(title = "Difference between Treated and Control Ridership",
       x = "Year",
       y = "Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("images/coeff_plot.png")


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

