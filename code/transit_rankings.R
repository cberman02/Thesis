transit_rankings <- read_csv(paste0("data/", 'transit_rankings.csv')) %>%
  rename(uza_name = Name) %>%
  mutate(uza_name = str_replace(uza_name, ",$", "")) #from https://alltransit.cnt.org/rankings/

all_uza_collapsed <- read_csv(paste0("data/clean/", 'all_uza.csv')) %>%
  group_by(uza_name) %>%
  dplyr::select(-year) %>%
  summarise(across(c(med_age, pop, white, black, asian, hispanic, poverty, med_house_income, bach), mean, na.rm = TRUE))

uza_info <- merge(all_uza_collapsed, transit_rankings, by = "uza_name")

write_csv(uza_info, paste0("data/clean/", 'complete_uza_info.csv'))
write_csv(all_uza_collapsed, paste0("data/clean/", 'uza_collapsed.csv'))
