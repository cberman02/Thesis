pinellas_pre <- read_csv(paste0("stop level/pinellas/", 'October 2019 Booking Ridership.csv')) %>%
  na.omit()
pinellas_post <- read_excel(paste0("stop level/pinellas/", 'final Feb 2020 Booking data.xlsx')) %>%
  clean_names()

#Creating plots
fl <- counties("Florida", cb = TRUE)
pinellas <- fl %>%
  filter(NAMELSAD == "Pinellas County")
hillsborough <- fl %>%
  filter(NAMELSAD == "Hillsborough County")


total_ridership <- sum(pinellas_pre$Final)
pinellas_pre$perc <- sqrt((pinellas_pre$Final / total_ridership) * 100)
total_ridership_post <- sum(pinellas_post$average_of_final)
pinellas_post$perc <- sqrt((pinellas_post$average_of_final / total_ridership_post) * 100)

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
