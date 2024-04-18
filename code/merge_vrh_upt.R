vrh <- read_csv("data/clean/vrh_data.csv") %>%
  select(agency, date, vrh, mode)
upt <- read_csv("data/clean/upt_data.csv")

#Merging 2016-2019 data by geoid
ntd_data <- upt %>%
  inner_join(vrh, by = c("agency" = "agency", "date" = "date", "mode" = "mode")) %>%
  unique()

write.csv(ntd_data, "data/clean/ntd_data.csv", row.names = FALSE) #Saving full data
