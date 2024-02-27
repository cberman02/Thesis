#Downloading all transit stops nationwide
transit_stops <- read_csv(paste0("data/raw/",'National_Transit_Map_Stops.csv')) %>%
  select(stop_lat, stop_lon, ntd_id, stop_name, location_type, parent_station) %>%
  mutate(transit_type = case_when((location_type == 0 | is.na(location_type)) & is.na(parent_station) ~ "Bus", 
                                  (location_type == 0 | is.na(location_type)) ~ "Rail",
                                  location_type == 1 ~ "Rail",
                                  location_type == 2 ~ "Rail",
                                  location_type == 3 ~ "Generic",
                                  location_type == 4 ~ "Bus")) #Imprecise. Might need to use transit_routes?

#Look into GTFS. Likely too complicated for this project



#Limiting to DART transit
transit_stops_dallas <- transit_stops %>%
  filter(ntd_id == 60056 & (transit_type == "Bus" | transit_type == "Rail"))

#Getting all unique agencies
agencies <- read_csv(paste0("data/",'transit_data.csv')) %>%
  #select(-c(date, month, year, ridership, mode, tos)) %>%
  select(ntd_id, agency, uza_name,x3_mode) %>%
  rename(transit_type = x3_mode) %>%
  unique()

uza_sf <- urban_areas() %>%
  rename(uza_name = NAME10) %>%
  mutate(uza_name = ifelse(uza_name == "Dallas--Fort Worth--Arlington, TX", "Dallas Worth, TX", uza_name))

#uza_names <- uza_sf %>%
 # select(uza_name)

#Merging agency info with their associated stops
dallas_stops <- merge(transit_stops_dallas, agencies, by = c("ntd_id", "transit_type"))

transit_sf <- merge(dallas_stops, uza_sf, by = "uza_name")

#write_csv(transit_sf, paste0("data/clean/",'transit_sf.csv'))
write_csv(dallas_stops, paste0("data/clean/",'transit_dallas.csv'))
#Takes too long to load sf to csv so just redownload I guess
#write_csv(uza_sf, paste0("data/clean/",'uza_sf.csv'))



