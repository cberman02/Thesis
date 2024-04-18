# You will be prompted for your Census API key here for replication. 
#This can be obtained at https://api.census.gov/data/key_signup.html
census_api_key = readline("Enter your Census API key:")
Sys.setenv("CENSUS_KEY" = census_api_key)


# Specify the geography and survey
geography <- "urban area"
survey <- "acs1"

# Variables of interest for Urbanized Areas
vars <- c(
  "B01003_001",  # Total population
  "B19013_001",   # Median household income
  "B17005_002", # Poverty Status
  "B23006_023", # Bachelors Degree or higher
  "B01002_001", # Median Age
  "B02001_002", # White Alone
  "B02001_003", # Black or African American Alone
  "B02001_005", # Asian Alone
  "B03001_003", # Hispanic or Latino Origin
  "B08015_001" #Number of vehicles used in commuting
)

# Years from 2010 to 2019
years <- 2010:2019

# Initialize an empty data frame to store results
all_uza <- data.frame()

# Loop through years and download Urbanized Area data
for (year in years) {
  # Download Urbanized Area data for the current year
  uza_data <- get_acs(
    geography = geography,
    variables = vars,
    year = year,
    survey = survey
  )
  uza_data$year <- year
  
  # Append the current year's data to the overall data frame
  all_uza <- bind_rows(all_uza, uza_data)
}

#Converting datatable to wide
all_uza <- all_uza %>%
  pivot_wider(
    id_cols = c("GEOID", "NAME", "year"),
    names_from = "variable",
    values_from = "estimate"
  )

colnames(all_uza) <- c("geoid","uza_name","year","med_age", "pop","white","black","asian","hispanic","poverty","med_house_income","bach","num_vehic_commute")

all_uza <- all_uza %>%
  mutate(across(c(hispanic, white, black, asian), ~ifelse(is.na(.x), 0, .x)))


#Converting below vars to proportions
all_uza$white <- all_uza$white / all_uza$pop
all_uza$black <- all_uza$black / all_uza$pop
all_uza$asian <- all_uza$asian / all_uza$pop
all_uza$hispanic <- all_uza$hispanic / all_uza$pop
all_uza$poverty <- all_uza$poverty / all_uza$pop

write_csv(all_uza, "data/clean/census.csv")
