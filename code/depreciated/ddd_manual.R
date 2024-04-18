pre_covid <- read_csv("data/pre_covid_transit.csv") %>%
  filter(year >= 2016)

placebo_agencies <- c("Hillsborough Area Regional Transit Authority",
                      "City of Durham",
                      "The Eastern Contra Costa Transit Authority")
pre_covid$placebo <- ifelse(pre_covid$agency %in% placebo_agencies, 1, 0)
pre_covid$treated <- ifelse(pre_covid$agency %in% placebo_agencies, 0, pre_covid$treated)


# Remove duplicate observations based on date, agency, and ridership
unique_indices <- !duplicated(pre_covid[, c("date", "agency", "ridership")])

# Subset the dataset to keep only unique observations
pre_covid_unique <- pre_covid[unique_indices, ]


placebo <- pre_covid_unique %>%
  filter(treated != 1 | placebo ==1)
treatment <- pre_covid_unique %>%
  filter(placebo == 0)

treated_ddd <- feols(log(ridership) ~ treated + time + treated:time, data = treatment)
placebo_ddd <- feols(log(ridership) ~ treated + time + treated:time, data = placebo)
ddd_try <- feols(log(ridership) ~ treated + time + treated:time + placebo + treated:placebo + time:placebo + treated:time:placebo, data = pre_covid_unique)
#ATT: -1.12646





ddd <- feols(log(ridership) ~ time + placebo + treated + time:placebo + time:treated + placebo:treated +
               time:treated:placebo, data = pre_covid_unique)
correlation_matrix <- cor(model.matrix(ddd))

