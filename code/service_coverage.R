census_df <- read_csv(paste0("data/clean/",'census_transit.csv')) %>%
  dplyr::select(agency, pop, uza_name,date) %>%
  filter(uza_name == "Dallas Worth, TX" & date == "2018-12-01")

# Read CSV file and create an sf object
transit_sf <- read_csv(paste0("data/clean/", 'transit_dallas.csv'))

#Calculating area serviced of transit station
transit_sf$area <- ifelse(transit_sf$transit_type == "Bus", 316, 1264)  # 0.25 miles or 0.5 miles in meters

dallas_uza <- urban_areas() %>%
  rename(uza_name = NAME10, sq_m = ALAND10) %>%
  filter(uza_name == "Dallas--Fort Worth--Arlington, TX") %>%
  dplyr::select(uza_name, sq_m) %>%
  mutate(uza_name = ifelse(uza_name == "Dallas--Fort Worth--Arlington, TX", "Dallas Worth, TX", uza_name))

#Calculating the service area of the UZA
dallas_uza$service_area <- sum(transit_sf$area)/dallas_uza$sq_m

#Map of dallas worth UZA plot
ggplot(dallas_uza) + 
  geom_sf(fill = "black", alpha = 0.8, col = "white") + 
  geom_sf(data = transit_sf, col = "red") +
  theme_bw()
