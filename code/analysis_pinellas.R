analysis <- read_csv("data/census_transit.csv") 

analysis_fl <- analysis%>%
  filter(state == "FL" & x3_mode == "Bus")

ggplot(analysis_fl, aes(x = ridership/1000)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Ridership Histogram", x = "Ridership (in 1000s)", y = "Frequency")

#Altamonte Springs serviced by Central Florida Regional Transportation Authority for bus and Central Florida Commuter Rail
#for train
print(unique(analysis_fl$agency))

#Removing outliers
analysis_fl <- analysis_fl %>%
  filter(ridership < 5000000)

ggplot(analysis_fl, aes(x = ridership/1000)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Ridership Histogram", x = "Ridership (in 1000s)", y = "Frequency")

#Adding treatment and time dummy vars
analysis_fl$treated <- ifelse(analysis_fl$agency == "Pinellas Suncoast Transit Authority", 1, 0)
analysis_fl$time = ifelse(analysis_fl$date >= "2017-01-19", 1, 0) #Launch of Pinellas FL Direct Connect

#Mode dummy vars
#analysis_fl$bus <- ifelse(analysis_fl$x3_mode == "Bus", 1, 0)
#analysis_fl$rail <- ifelse(analysis_fl$x3_mode == "Rail", 1, 0)

#Regression for subsidy in Pinellas county
didreg <- feols(
  fml = ridership ~ pop + white + med_house_income + poverty + treated + time + treated*time,
  vcov = "iid",
  data = analysis_fl
)
didreg_fe <- feols(
  fml = ridership ~ pop + white + med_house_income + poverty + treated + time + treated*time | month,
  vcov = "iid",
  data = analysis_fl
)
etable(
  didreg, didreg_fe, 
  coefstat = "tstat", digits = 3, digits.stats = 3
)
