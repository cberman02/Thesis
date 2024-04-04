pre_covid <- read_csv("data/pre_covid_transit.csv")
placebo_agencies <- c("Hillsborough Area Regional Transit Authority",
                      "The Eastern Contra Costa Transit Authority",
                      "City of Durham")
pre_covid$placebo <- ifelse(pre_covid$agency == placebo_agencies,1, 0)

pre_covid_abbr <- pre_covid %>%
  filter(year >= 2016)

did_treated <- feols(log(ridership) ~ treated:time | agency + date, data = pre_covid_abbr) #(1)
did_placebo <- feols(log(ridership) ~ placebo:time | agency + date, data = pre_covid_abbr) #(2)

pre_covid_abbr$treated <- ifelse(pre_covid_abbr$agency == placebo_agencies,1, pre_covid_abbr$treated) #Adding in fake treated

ddd <- feols(log(ridership) ~ treated:time:placebo | agency + date, data = pre_covid_abbr) #(3)

#Trying different fixed effects
ddd_fe_1 <- feols(log(ridership) ~ treated:time:placebo | agency^date, data = pre_covid_abbr) #No statistical significance, R^2 of 1
ddd_fe_2 <- feols(log(ridership) ~ treated:time:placebo | uza_name^date, data = pre_covid_abbr) #Very high coefficient
ddd_fe_3 <- feols(log(ridership) ~ treated:time:placebo | agency + date + agency^date, data = pre_covid_abbr) #R^2 of 1
ddd_fe_4 <- feols(log(ridership) ~ treated:time:placebo | agency + date + uza_name^date, data = pre_covid_abbr) #No statistical significance

print(etable(did_treated, dd_placebo, ddd, tex = T))
print(etable(ddd_fe_1, ddd_fe_2, ddd_fe_3, ddd_fe_4,tex = T))


#pre_covid$treated <- ifelse(pre_covid$agency == placebo_agencies,0, pre_covid$treated) #Setting treated and placebo to normal


#Try adding placebo to treatment and run expanded form to check for robustness

#So for the table, (1) is standard DID (treatment and control), (2) is placebo DID (placebo and control), 
#and (3) is the triple difference