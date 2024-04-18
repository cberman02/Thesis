all_transit_census <- read_csv("data/clean/ntd_census.csv")
reg_output <- read_csv("tables/reg_output.csv")
sunab_did <- feols(ridership_pop ~pop_100000+ med_inc_10000 + 
                     sunab(month_treat, time_to_treat, ref.c = (0), ref.p = c(-1,-1000)) | 
                     agency + date,data = all_transit_census) #Used for some iplot commands

#----------------------------------------------------------------------------------------------------
# Figure 1: Plot of per capita ridership for treated and control

#Creating
ridership_treated <- all_transit_census %>%
  filter(treated == 1) %>%
  group_by(year) %>%
  summarise(average_ridership_per_capita = round(mean(ridership_pop, na.rm = TRUE), digits = 4))

ridership_control <- all_transit_census %>%
  filter(treated == 0) %>%
  group_by(year) %>%
  summarise(average_ridership_per_capita = round(mean(ridership_pop, na.rm = TRUE), digits = 4))

#Plot of per capita ridership
ggplot() +
  geom_line(data = ridership_treated, aes(x = year, y = average_ridership_per_capita, color = "Treated")) +
  geom_line(data = ridership_control, aes(x = year, y = average_ridership_per_capita, color = "Control")) +
  labs(
    x = "Year",
    y = "Average Ridership per Capita"
  ) + 
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Treated", "Control"),
                     name = "Treatment Status") +
  theme_minimal()
ggsave("figures/fig_1_ridership_per_capita_treat_control.png")
#----------------------------------------------------------------------------------------------------
# Figure 2: Distribution of demographic vars between treat and control

#Creating avgs for plots
agency_summary <- all_transit_census %>%
  group_by(agency) %>%
  summarise(
    average_population = mean(pop_100000, na.rm = TRUE),
    avg_vrh = mean(vrh, na.rm = TRUE),
    average_ridership = mean(ridership_pop, na.rm = TRUE),
    treated_status = first(treated),
    avg_white = mean(white, na.rm = TRUE),
    avg_black = mean(black, na.rm = TRUE),
    avg_hispanic = mean(hispanic, na.rm = TRUE),
    avg_asian = mean(asian, na.rm = TRUE),
    avg_age = mean(med_age, na.rm = TRUE),
    avg_income = mean(med_inc_10000, na.rm = TRUE)
  ) %>%
  mutate(treated_status = ifelse(treated_status == 1, "Treated", "Control"))

#Pop
a <- ggplot(agency_summary, aes(x = average_population, y = factor(treated_status), fill = factor(treated_status))) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,) +
  labs(x = "Average Population (in 100,000s)", y = "Treated Status", title = "UZA Population") +
  theme(legend.position = "none",axis.title.x = element_text(size = 8))
#Ridership
b<- ggplot(agency_summary, aes(x = average_ridership, y = factor(treated_status), fill = factor(treated_status))) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,) +
  labs(x = "Average Ridership per Capita", y = "", title = "Agency Ridership") +
  theme(legend.position = "none",axis.title.x = element_text(size = 8))
#Race
c <- ggplot(agency_summary, aes(x = avg_white, y = factor(treated_status), fill = factor(treated_status))) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,) +
  labs(x = "Average White Population", y = "", title = "UZA Perc. White") +
  theme(legend.position = "none",axis.title.x = element_text(size = 8))
#Age
d <- ggplot(agency_summary, aes(x = avg_age, y = factor(treated_status), fill = factor(treated_status))) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,) +
  labs(x = "Median Age", y = "Treated Status", title = "UZA Median Age") +
  theme(legend.position = "none",axis.title.x = element_text(size = 8))
#Income
e <- ggplot(agency_summary, aes(x = avg_income, y = factor(treated_status), fill = factor(treated_status))) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,) +
  labs(x = "Median Household Income", y = "", title = "UZA Median Household Income") +
  theme(legend.position = "none",axis.title.x = element_text(size = 8))

combined_plots <- grid.arrange(a,b,c,d,e, nrow = 2)

# Export the combined plot as one image
ggsave("figures/fig_2_combined_demographic_plots.png", combined_plots,width = 6.5, height = 4.5)

#----------------------------------------------------------------------------------------------------
#Figure 3: Coefficient Plot

#Plotting
pdf("figures/fig_3_coef_plot.pdf")
iplot(sunab_did, 
      drop     = "[[:digit:]]{2}", 
      xlab     = "Time to Treatment",
      main     = "",
      ref.line      = 0
)
dev.off()
#----------------------------------------------------------------------------------------------------
#Figure 4: Ridership volatility
ggplot(subset(all_transit_census, treated == 1), aes(x = date, y = ridership_pop, color = factor(agency))) +
  geom_line() +
  labs(
    x = "Date",
    y = "Ridership per capita") +
  scale_color_discrete(name = "Agency") +
  theme_minimal()
ggsave("figures/fig_4_ridership_volatility_plot.png", width = 8, height = 6, units = "in")

#----------------------------------------------------------------------------------------------------
#Figure 5: Cohort and ATT Coefficient Plot
agg_coef_cohort <- reg_output
agg_coef_cohort$group <- c("Pinellas Suncoast Transit Authority",
                           "Livermore/Amador Valley Transit Authority",
                           "Orange County Transportation Authority",
                           "Solano County Transit",
                           "Greater Dayton Regional Transit Authority",
                           "City of Charlotte North Carolina",
                           "Pierce County and Detroit",
                           "Research Triangle",
                           "ATT")
agg_coef_cohort$ci_low <- agg_coef_cohort$Estimate - 1.96 * agg_coef_cohort$`Std. Error`
agg_coef_cohort$ci_hi <- agg_coef_cohort$Estimate + 1.96 * agg_coef_cohort$`Std. Error`

#Coefficient plot of cohort
ggplot(agg_coef_cohort, aes(x=Estimate, y=group)) + 
  geom_point(col = 4) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_hi), height = 0.1, col = 4, size = 0.75) +
  labs(y = "Cohort") +
  geom_vline(xintercept = 0) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Hard lines around the plot
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    plot.background = element_rect(fill = "white"),  # Set background color to white
    panel.background = element_rect(fill = "white") 
  )
ggsave("figures/fig_5_cohort_coef_plot.png")

#----------------------------------------------------------------------------------------------------
#Figure 6: Placebo Coefficient Plot
all_transit_census$vrh_pop <- all_transit_census$vrh/all_transit_census$pop
sunab_placebo<- feols(vrh ~pop_100000+ med_inc_10000 + 
                        sunab(month_treat, time_to_treat, ref.c = (1000), ref.p = c(-1,-1000)) | 
                        agency + date,data = all_transit_census) #Used for plot, table does not appear
# summary(sunab_placebo)
# aggregate(sunab_placebo, "cohort")
# aggregate(sunab_placebo, "ATT")

pdf("figures/fig_6_coef_plot_placebo.pdf")
iplot(sunab_placebo, 
      drop     = "[[:digit:]]{2}", 
      xlab     = "Time to Treatment",
      ref.line      = 0
)
dev.off()

#----------------------------------------------------------------------------------------------------

