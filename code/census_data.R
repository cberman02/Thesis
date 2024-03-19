#API Key: 3528235a2cdd9715af39bec20ec64ea0c85f9c4c
# Set your Census API key
Sys.setenv("CENSUS_KEY" = "3528235a2cdd9715af39bec20ec64ea0c85f9c4c")


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

# Years from 2005 to present
years <- 2006:2022
years <- years[years != 2020] #2020 data is missing

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

colnames(all_uza) <- c("geoid","uza_name","year","med_age", "pop","white","black","asian","hispanic","poverty","med_house_income","bach")

#Converting below vars to proportions
all_uza$white <- all_uza$white / all_uza$pop
all_uza$black <- all_uza$black / all_uza$pop
all_uza$asian <- all_uza$asian / all_uza$pop
all_uza$hispanic <- all_uza$hispanic / all_uza$pop
all_uza$poverty <- all_uza$poverty / all_uza$pop

write_csv(all_uza, "data/all_uza.csv")
