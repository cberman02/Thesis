analysis <- read_csv("data/census_transit.csv")
#analysis$rides_pop <- analysis$ridership/analysis$pop

#Creating average ridership stats for plotting
analysis_sum <- analysis %>%
  group_by(uza_name) %>%
  summarise(
    avg_pop = mean(pop),
    avg_ride = mean(ridership),
    ride_capita = mean(avg_ride / avg_pop)
  )
quartiles <- quantile(analysis_sum$ride_capita, probs = c(0.25, 0.5, 0.75))

#head(analysis_sum)

#A closer look at subject cities
dallas <- analysis %>%
  filter(uza_name == "Dallas Worth, TX" &
           agency == "Dallas Area Rapid Transit") #Limiting to DART which integrated app (10/26/15)
#https://www.dart.org/about/news-and-events/newsreleases/newsrelease-detail/dart--lyft-creating-new-transit-choices-1213
dallas_mean <- mean(dallas$ridership)

atlanta <- analysis %>%
  filter(uza_name == "Atlanta, GA" &
           agency == "Metropolitan Atlanta Rapid Transit Authority") #Limiting to MARTA which integrated app (7/23/15)
#https://www.wsbtv.com/news/local/marta-partners-uber-adds-wi-fi-buses/33425046/
atlanta_mean <- mean(atlanta$ridership)

ggplot(analysis, aes(x = ridership/1000)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Ridership Histogram", x = "Ridership (in 1000s)", y = "Frequency") +
  geom_vline(xintercept = dallas_mean/1000, color = "red") +
  geom_vline(xintercept = atlanta_mean/1000, color = "blue") +
  xlim(0, 20000) +
  ylim(0,75000)


#Graphs for ridership/capita
ggplot(analysis_sum, aes(y = avg_pop/1000, x = ride_capita)) +
  geom_point(alpha = 0.5) +
  labs(title = "Ridership vs Population", x = "Ridership per Capita", y = "Population (in 1000s)") +
  #ylim(0, 20000) +
  geom_smooth(method = "lm", se = TRUE)
ggplot(analysis_sum, aes(x = ride_capita)) +
  geom_histogram(binwidth = 0.10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Rides/Capita Histogram", x = "Rides/Capita", y = "Frequency") +
  geom_vline(xintercept = quartiles, color = "red")
  
