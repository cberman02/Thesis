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
#Creating table of t test values
t_test_df <- data.frame()
for (i in 2005:2020) {
  pre_covid_year <- pre_covid %>%
    filter(year == i)
  t_test_result <- t.test(log(ridership) ~ treated, pre_covid_year)
  t_test_df <- rbind(t_test_df, data.frame(year = i, coefficient = t_test_result$estimate, 
                                           lower_ci = t_test_result$conf.int[1], upper_ci = t_test_result$conf.int[2]))
}
#Bit of nonsense to remove duplicates and format the table nicely
t_test_year <- t_test_df %>%
  select(year, lower_ci, upper_ci) %>%
  unique()
t_test_df <- tibble::rowid_to_column(t_test_df, "group") %>%
  mutate(group = ifelse(group %% 2 != 0, "control", "treated"))
t_test_coeff <- t_test_df %>%
  select(year, coefficient, group) %>%
  pivot_wider(names_from = group, values_from = coefficient, names_prefix = "group")
t_test_df <- merge(t_test_coeff, t_test_year, by = "year") %>%
  rename(control = groupcontrol, treated = grouptreated)
t_test_df$diff <- t_test_df$control-t_test_df$treated
#xtable(t_test_df, type = "latex")

# Plot
ggplot(t_test_df, aes(x = factor(year))) +
  geom_point(aes(y = diff), color = "blue") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a line at y = 0
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

