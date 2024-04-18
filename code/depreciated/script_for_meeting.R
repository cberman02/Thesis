source("code/triple_diff.R") #Initial regressions

#Creating avg ridership plots
avg_ridership_treated <- pre_covid %>%
  filter(treated == 1 & placebo == 0) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log(ridership)))
avg_ridership_placebo <- pre_covid %>%
  filter(placebo == 1) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log(ridership)))
avg_ridership_control <- pre_covid %>%
  filter(treated == 0 & placebo == 0) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log(ridership)))
avg_riders <- merge(avg_ridership_treated, avg_ridership_placebo, by = "date")
avg_riders <- merge(avg_riders, avg_ridership_control, by = "date") %>%
  rename("treated" = log_ridership.x,
         "placebo" = log_ridership.y,
         "control" = log_ridership)
#Graphs for checking
three_groups <- ggplot() +
  geom_line(data = avg_riders, aes(x = date, y = placebo, color = "Placebo")) +
  geom_line(data = avg_riders, aes(x = date, y = treated, color = "Treated")) +
  geom_line(data = avg_riders, aes(x = date, y = control, color = "Control")) +
  labs(title = "Ridership",
       x = "Date",
       y = "Average Ridership",
       color = "Treatment Status") +
  theme_minimal()


avg_riders_abbr <- avg_riders %>%
  filter(date >= "2016-01-01")
three_groups_abbr <- ggplot() +
  geom_line(data = avg_riders_abbr, aes(x = date, y = placebo, color = "Placebo")) +
  geom_line(data = avg_riders_abbr, aes(x = date, y = treated, color = "Treated")) +
  geom_line(data = avg_riders_abbr, aes(x = date, y = control, color = "Control")) +
  labs(title = "Transit Agency Groups Ridership from 2016-2020",
       x = "Date",
       y = "Average Ridership",
       color = "Treatment Status") +
  theme_minimal()

b <- arrangeGrob(three_groups, three_groups_abbr, nrow=2) #generates g
ggsave("images/ridership_by_group.png", b, width = 8, height = 8, units = "in", dpi = 300)



placebo_rides <- ggplot(data = pre_covid, aes(x = date, y = log(ridership), color = agency)) +
  geom_line(data = subset(pre_covid, placebo == 1)) +
  labs(title = "Placebo Agency Ridership",
       x = "Date",
       y = "Average Ridership",
       color = "Agency") +
  theme_minimal()
treated_rides <- ggplot(data = pre_covid, aes(x = date, y = log(ridership), color = agency)) +
  geom_line(data = subset(pre_covid, treated == 1)) +
  labs(title = "Treated Agency Ridership",
       x = "Date",
       y = "Average Ridership",
       color = "Agency") +
  theme_minimal()
placebo_rides_abbr <- ggplot(data = pre_covid, aes(x = date, y = log(ridership), color = agency)) +
  geom_line(data = subset(pre_covid, placebo == 1 & year >= 2016)) +
  labs(title = "Placebo Agency Ridership 2016-2020",
       x = "Date",
       y = "Average Ridership",
       color = "Agency") +
  theme_minimal()
treated_rides_abbr <- ggplot(data = pre_covid, aes(x = date, y = log(ridership), color = agency)) +
  geom_line(data = subset(pre_covid, treated == 1 & year >= 2016)) +
  labs(title = "Treated Agency Ridership 2016-2020",
       x = "Date",
       y = "Average Ridership",
       color = "Agency") +
  theme_minimal()
g <- arrangeGrob(placebo_rides, treated_rides, placebo_rides_abbr, treated_rides_abbr, nrow=4) #generates g
ggsave("images/treated_placebo_ridership.png", g, width = 8, height = 8, units = "in", dpi = 300)
