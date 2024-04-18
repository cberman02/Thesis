all_transit <- read_csv("data/clean/ntd_data.csv") %>%
  filter(year >= 2014 & year <= 2019)

unique_indices <- !duplicated(all_transit[, c("date", "agency", "ridership")])
#Removing expired programs
all_transit <- all_transit[unique_indices, ] %>%
  filter(uza_name != "Austin, TX" | 
           uza_name != "Philadelphia, PA" |
           uza_name != "Phoenix, AZ")
treated_agencies <- c("Livermore / Amador Valley Transit Authority",
                      "City of Charlotte North Carolina" ,
                      "Greater Dayton Regional Transit Authority",
                      "Pinellas Suncoast Transit Authority",
                      "Orange County Transportation Authority",
                      "Pierce County Transportation Benefit Area Authority",
                      "Solano County Transit",
                      "City of Detroit",
                      "Research Triangle Regional Public Transportation Authority")
control_agencies <- all_transit %>%
  filter(!(agency %in% treated_agencies)) %>%
  select(agency) %>%
  unique

all_transit$treated <- ifelse(all_transit$agency %in% treated_agencies, 1, 0)

#Should probably use case_when instead of ifelse
all_transit$post <- ifelse(all_transit$agency == "Livermore / Amador Valley Transit Authority" & all_transit$date >= "2016-09-01", 1,
                           ifelse(all_transit$agency == "City of Charlotte North Carolina" & all_transit$date >= "2018-04-01", 1,
                                  ifelse(all_transit$agency == "Greater Dayton Regional Transit Authority" & all_transit$date >= "2017-06-01", 1,
                                         ifelse(all_transit$agency == "Pinellas Suncoast Transit Authority" & all_transit$date >= "2016-02-01", 1,
                                                ifelse(all_transit$agency == "Orange County Transportation Authority" & all_transit$date >= "2016-10-01", 1,
                                                       ifelse(all_transit$agency == "Pierce County Transportation Benefit Area Authority" & all_transit$date >= "2018-05-01", 1,
                                                              ifelse(all_transit$agency == "Solano County Transit" & all_transit$date >= "2017-05-01", 1,
                                                                     ifelse(all_transit$agency == "City of Detroit" & all_transit$date >= "2018-05-01", 1,
                                                                            ifelse(all_transit$agency == "Research Triangle Regional Public Transportation Authority"
                                                                                   & all_transit$date >= "2019-12-01",1,0)))))))))


all_transit$log_ridership <- log(all_transit$ridership)

#Creating month number and months to legalization
month_num <- rep(1:length(unique(all_transit$date)))
months <- unique(all_transit$date)
month_counter <- data.frame(month_num, months) %>%
  rename(date = months)
all_transit <- merge(all_transit, month_counter, by = "date")

date_treat <- all_transit %>%
  filter(treated == 1 & post == 1) %>%
  select(agency, month_num) %>%
  rename(month_treat = month_num) %>%
  distinct(by=agency, .keep_all = T) %>%
  select(-by)

treatment_times <- date_treat$month_treated #For treatment times

date_default <- data.frame(control_agencies, rep(0, length(control_agencies))) %>% 
  rename(month_treat = rep.0..length.control_agencies..)
date_treat <- rbind(date_treat, date_default)

all_transit <- merge(all_transit, date_treat, by = "agency")

all_transit$time_to_treat <- ifelse(all_transit$month_treat > 0, all_transit$month_num - all_transit$month_treat, -1000)

write_csv(all_transit, "data/clean/treated_ntd.csv")
