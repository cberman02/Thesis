ntd_census <- read_csv("data/clean/ntd_census.csv")
# ntd_census cleaning and preprocessing
# Handle missing values, ntd_census type conversions, etc.

# Variable Definitions
# variable_definitions <- data(
#   variable_name = c("var1", "var2", ...),
#   definition = c("Definition of var1", "Definition of var2", ...),
# )

summary_stats <- st(ntd_census, out = "latex", file = "tables/full_sum_stats.tex")

# Iterate over each variable
histogram_list <- list()

# List of variables
num_var_list <- c("ridership", "log_ridership", "ridership_pop", "uza_ridership", "state_ridership", 
              "vrh", "med_age", "pop", "pop_100000", "white", "black", "asian", "hispanic", 
              "poverty", "med_house_income", "med_inc_10000", "num_vehic_commute", "vehic_per_capita")
cat_var_list <- c("year","x3_mode","reporter_type")
weird_ones <- c("state", "uza_name")

# Open a PDF device for plotting
pdf("figures/data_appendix_histogram_grid.pdf", width = 6.5, height = 9)  # Change dimensions as needed
# Set up a multi-panel layout
par(mfrow = c(6, 3))
# Loop over each variable
for (i in 1:length(num_var_list)) {
  variable <- num_var_list[i]
  # Check if the variable is numeric
  if (is.numeric(ntd_census[[variable]])) {
    # Create histogram
    hist(ntd_census[[variable]], main = paste("Histogram of", variable), xlab = variable)
  }
}
# Close the PDF device
dev.off()

# Open a PDF device for plotting
pdf("figures/data_appendix_bar_grid.pdf", width = 6.5, height = 9)  # Change dimensions as needed

# Set up a multi-panel layout
par(mfrow = c(3, 2))

# Loop over each variable
for (variable in cat_var_list) {
  # Check if the variable is categorical
    barplot(table(ntd_census[[variable]]), main = paste("Freq. Bar Plot of", variable), 
              xlab = variable, ylab = "Frequency")
}
for (variable in weird_ones) {
  # Check if the variable is categorical
  freq_table <- table(ntd_census[[variable]])
  top_options <- sort(freq_table, decreasing = TRUE)[1:5]
  bottom_options <- sort(freq_table, decreasing = FALSE)[1:5]
  barplot(table(ntd_census[[variable]]), main = paste("Freq. Bar Plot of", variable), 
          xlab = variable, ylab = "Frequency")
}
# Close the PDF device
dev.off()