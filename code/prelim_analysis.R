transit_data <- read.csv("data/transit_data.csv") %>%
  separate(col = uza_name, into = c("city", "state"), sep = ", ", remove = FALSE)

#For simple regression (all below agencies are 2020 DiD)
# transit_data$treated <- ifelse(transit_data$agency %in% 
#                                  c("Dallas Area Rapid Transit", 
#                                  "Bi-State Development Agency of the Missouri-Illinois Metropolitan District",
#                                  "Pinellas Suncoast Transit Authority",
#                                  "Livermore / Amador Valley Transit Authority",
#                                  "Research Triangle Regional Public Transportation Authority"), 1, 0) 
#Livermore and RTP 12/2019, PSTA 11/2019, just treating as 2020

#transit_data$time <- ifelse(transit_data$year > 2020, 1, 0)

#transit_data$treated_time <- transit_data$treated * transit_data$time

transit_data$treated <- ifelse(transit_data$agency %in%
                                  c("Pinellas Suncoast Transit Authority",
                                  "Livermore / Amador Valley Transit Authority",
                                  "Research Triangle Regional Public Transportation Authority"), 1, 0)
transit_data <- transit_data %>%
  filter(date < "2020-03-01" & date >= "2016-01-01" & ridership > 0) #Restricted to before COVID
transit_data$time <- ifelse(transit_data$date > "2019-12-01", 1, 0)

# DiD regression with fixed effects using feols
did_base <- feols(ridership ~ treated:time, data = transit_data)
did_fe <- feols(ridership ~ treated:time | month + state, data = transit_data)
did_ln <- feols(log(ridership) ~ treated:time | month + state, data = transit_data)

# Summarize the results
summary(did_base)
summary(did_fe)
summary(did_ln)

etable(did_base, did_fe, did_ln, tex=TRUE)
