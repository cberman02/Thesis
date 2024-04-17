pinellas_pre <- read_csv(paste0("stop level/pinellas/", 'October 2019 Booking Ridership.csv')) %>%
  na.omit() %>%
  clean_names() %>%
  rename(off_2019)

pinellas_post <- read_excel(paste0("stop level/pinellas/", 'final Feb 2020 Booking data.xlsx')) %>%
  clean_names() %>%
  select(stop_id, stop_name, average_of_off, average_of_on,average_of_final,lat,long,rank) %>%
  rename(off_2020 = average_of_off, on_2020 = average_of_on, final_2020 = average_of_final, rank_2020 = rank)

pinellas_stops <- merge(pinellas_pre, pinellas_post, by = c("stop_id","stop_name", "lat", "long"))

#Creating plots
fl <- counties("Florida", cb = TRUE)
pinellas <- fl %>%
  filter(NAMELSAD == "Pinellas County")



# Plot
ggplot() +
  geom_sf(data = pinellas, fill = "transparent", color = "black") +  # Plot UZA shapefile
  #geom_sf(data = hillsborough, fill = "transparent", color = "black") +  # Plot UZA shapefile
  geom_point(data = pinellas_pre, aes(x = LONG, y = LAT, fill = perc), shape = 21, size = 2, alpha = 0.7) +  # Plot stops
  scale_fill_gradient(low = "green", high = "red", name = "Percentage of Ridership") +  # Gradient legend
  labs(title = "Transit Stops in St. Petersburg-Tampa UZA", x = "Longitude", y = "Latitude") +  # Labels
  theme_minimal()  # Minimal theme

ggplot() +
  geom_sf(data = pinellas_uza, fill = "transparent", color = "black") +  # Plot UZA shapefile
  geom_point(data = pinellas_post, aes(x = long, y = lat, fill = perc), shape = 21, size = 2, alpha = 0.5) +  # Plot stops
  scale_fill_gradient(low = "green", high = "red", name = "Percentage of Ridership") +  # Gradient legend
  labs(title = "Transit Stops in St. Petersburg-Tampa UZA", x = "Longitude", y = "Latitude") +  # Labels
  theme_minimal()  # Minimal theme







#Testing 
# Perform k-means clustering based on latitude and longitude
# Perform k-means clustering
num_clusters <- 10  # Adjust the number of clusters as needed
cluster_centers <- kmeans(pinellas_pre[, c("LAT", "LONG")], centers = num_clusters)$centers

# Assign cluster labels to each point in the original dataset
pinellas_pre$cluster <- as.factor(kmeans(pinellas_pre[, c("LAT", "LONG")], centers = cluster_centers)$cluster)

# Plot
ggplot() +
  geom_sf(data = uza_sf, fill = "transparent", color = "black") +  # Plot UZA shapefile
  geom_point(data = pinellas_pre, aes(x = Longitude, y = Latitude, fill = Total_Ridership), shape = 21, size = 2, alpha = 0.5) +  # Plot stops with smaller size and more transparency
  scale_fill_gradient(low = "green", high = "red", name = "Total Ridership") +  # Gradient legend
  labs(title = "Transit Stops in St. Petersburg-Tampa UZA", x = "Longitude", y = "Latitude") +  # Labels
  theme_minimal()  # Minimal theme
