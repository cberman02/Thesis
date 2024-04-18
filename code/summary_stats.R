ridership <- read.csv("data/clean/ntd_census.csv")

#Ridership sum stats
sink("tables/tab_1_ridership_sum_stats.txt")
st(ridership, vars = c("ridership","vrh", "ridership_pop", "x3_mode"), 
   out = "latex")
sink(file = NULL)

#Agency, UZA, state numbers added manually to tab_1
unique_agency <- length(unique(ridership$agency))
unique_treated <- length(unique(subset(ridership, treated == 1)$agency))
unique_control <- length(unique(subset(ridership, treated == 0)$agency))
unique_uza <- length(unique(ridership$uza_name))
unique_state <- length(unique(ridership$state))

print(paste("Unique agencies: ", unique_agency))
print(paste("Unique treated agencies: ", unique_treated))
print(paste("Unique control agencies: ", unique_control))
print(paste("Unique UZAs: ", unique_uza))
print(paste("Unique states: ", unique_state))


#Ridership by year
summary_stats <- ridership %>%
  group_by(year) %>%
  summarise(
    n = n(),
    average_ridership_per_capita = mean(ridership_pop, na.rm = TRUE),
    sd_ridership_per_capita = sd(ridership_pop, na.rm = TRUE),
    min_ridership_per_capita = min(ridership_pop, na.rm = TRUE),
    q25_ridership_per_capita = quantile(ridership_pop, 0.25, na.rm = TRUE),
    median_ridership_per_capita = median(ridership_pop, na.rm = TRUE),
    q75_ridership_per_capita = quantile(ridership_pop, 0.75, na.rm = TRUE),
    max_ridership_per_capita = max(ridership_pop, na.rm = TRUE)
  ) %>%
  arrange(year)
summary_stats_latex <- kable(summary_stats, format = "latex", booktabs = TRUE)
write(summary_stats_latex, file = "tables/tab_2_ridership_over_time.tex")


#Creating sum stats tables for NTD sorted by treated and control
ridership_treated <- ridership %>%
  filter(treated == 1) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    average_ridership_per_capita = round(mean(ridership_pop, na.rm = TRUE), digits = 4),
    sd_ridership_per_capita = round(sd(ridership_pop, na.rm = TRUE), digits = 4),
    min_ridership_per_capita = round(min(ridership_pop, na.rm = TRUE), digits = 4),
    q25_ridership_per_capita = round(quantile(ridership_pop, 0.25, na.rm = TRUE), digits = 4),
    median_ridership_per_capita = round(median(ridership_pop, na.rm = TRUE), digits = 4),
    q75_ridership_per_capita = round(quantile(ridership_pop, 0.75, na.rm = TRUE), digits = 4),
    max_ridership_per_capita = round(max(ridership_pop, na.rm = TRUE), digits = 4)
    ) %>%
  arrange(year)

ridership_control <- ridership %>%
  filter(treated == 0) %>%
  group_by(year) %>%
    summarise(
      n = n(),
      average_ridership_per_capita = round(mean(ridership_pop, na.rm = TRUE), digits = 4),
      sd_ridership_per_capita = round(sd(ridership_pop, na.rm = TRUE), digits = 4),
      min_ridership_per_capita = round(min(ridership_pop, na.rm = TRUE), digits = 4),
      q25_ridership_per_capita = round(quantile(ridership_pop, 0.25, na.rm = TRUE), digits = 4),
      median_ridership_per_capita = round(median(ridership_pop, na.rm = TRUE), digits = 4),
      q75_ridership_per_capita = round(quantile(ridership_pop, 0.75, na.rm = TRUE), digits = 4),
      max_ridership_per_capita = round(max(ridership_pop, na.rm = TRUE), digits = 4)
    ) %>%
    arrange(year)
write_csv(ridership_treated, "tables/tab_na_ntd_treated_sum_stats.csv")
write_csv(ridership_control, "tables/tab_na_ntd_control_sum_stats.csv")


#Sum stats for census
sink(file = "tables/tab_4_acs_sum_stats.txt")
st(ridership, vars = c("med_age", "pop", "white", "black", "asian", "hispanic", "med_house_income"), 
   out = "latex")
sink(file = NULL)
