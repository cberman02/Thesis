pre_covid <- read_csv("data/pre_covid.csv")
pre_covid_transit <- read_csv("data/pre_covid_transit.csv")
pre_covid_transit$placebo <- ifelse(pre_covid_transit$agency %in%
                                      c("Hillsborough Area Regional Transit Authority",
                                        "The Eastern Contra Costa Transit Authority",
                                        "Town of Chapel Hill"), 1, 0)
pre_covid_transit$log_ridership <- log(pre_covid_transit$ridership)

placebo_transit <- pre_covid_transit %>%
  filter(!(agency %in%
           c("Pinellas Suncoast Transit Authority",
             "Livermore / Amador Valley Transit Authority",
             "Research Triangle Regional Public Transportation Authority")))
placebo_transit$treated <- ifelse(placebo_transit$agency %in%
                            c("Hillsborough Area Regional Transit Authority",
                              "The Eastern Contra Costa Transit Authority",
                              "Town of Chapel Hill"), 1, 0)

did_fe <- feols(ridership ~ treated:time | agency + date, data = pre_covid_transit)
did_ln <- feols(log(ridership) ~ treated:time | agency + date, data = pre_covid_transit)
did_fe_placebo <- feols(ridership ~ treated:time | agency + date, data = placebo_transit)
did_ln_placebo <- feols(log(ridership) ~ treated:time | agency + date, data = placebo_transit)
etable(did_fe, did_ln, did_fe_placebo, did_ln_placebo, tex = T)

triple_diff <- feols(log(ridership) ~ treated+placebo+time, data = pre_covid_transit)
ddd <- lm(log(ridership) ~ treated*time*placebo, data = pre_covid_transit)

etable(triple_diff)

intercept <- triple_diff$coefficients[1]
treated <- triple_diff$coefficients[2]
placebo <- triple_diff$coefficients[3]
post <- triple_diff$coefficients[4]
estimator <- ((intercept + treated + placebo + post) - (intercept + treated + placebo)) -
             ((intercept + placebo + post) - (intercept + placebo)) -
             ((intercept + treated + post) - (intercept + treated)) -
             ((intercept + post) - (intercept))
