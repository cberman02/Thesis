transit_rankings <- read_csv(paste0("data/", 'transit_rankings.csv')) %>%
  clean_names() %>%
  rename(uza_name = name) %>%
  rename(transit_shed_mi2 = transit_shed) %>%
  mutate(uza_name = str_replace(uza_name, ",$", ""))
  #from https://alltransit.cnt.org/rankings/

transit_rankings$transit_shed_mi2 <- as.numeric(gsub("[^0-9.]", "", transit_rankings$transit_shed_mi2))
transit_rankings$percent_transit <- as.numeric(gsub("[^0-9.]", "", transit_rankings$percent_transit))

write_csv(transit_rankings, paste0("data/", 'transit_rankings.csv'))
