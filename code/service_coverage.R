census_df <- read_csv(paste0("data/clean/",'census_transit.csv')) %>%
  dplyr::select(agency, pop, uza_name,date) %>%
  filter(uza_name == "Dallas Worth, TX" & date == "2018-12-01")

# Read CSV file and create an sf object
transit_sf <- read_csv(paste0("data/clean/", 'transit_dallas.csv')) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Define buffer distances based on station type
buffer_distance <- ifelse(transit_sf$transit_type == "Bus", 402.335, 805.671)  # 0.25 miles or 0.5 miles in meters

# Create buffers directly on the geometry column
transit_sf$geometry <- st_buffer(transit_sf$geometry, dist = buffer_distance)

# Calculate and add buffer areas
transit_sf$area <- st_area(transit_sf$geometry)


dallas_uza <- urban_areas() %>%
  rename(uza_name = NAME10) %>%
  filter(uza_name == "Dallas--Fort Worth--Arlington, TX") %>%
  mutate(uza_name = ifelse(uza_name == "Dallas--Fort Worth--Arlington, TX", "Dallas Worth, TX", uza_name))

#Map of dallas worth UZA plot
ggplot(dallas_uza) + 
  geom_sf(fill = "black", alpha = 0.8, col = "white") + 
  geom_sf(data = transit_sf, col = "red") +
  theme_bw()
