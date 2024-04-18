all_transit_census <- read_csv("data/clean/ntd_census.csv") 


#Base reg
simple_did <- feols(ridership_pop ~ pop_100000 + med_inc_10000 + treated:post| agency + date,data = all_transit_census)
base_twfe <- feols(ridership_pop ~pop_100000 + med_inc_10000 + i(time_to_treat, treated,
                                     ref = c(-1, -1000)) | agency + date, all_transit_census) #For coef
summary(simple_did)

#Sunab reg
sunab_did <- feols(ridership_pop ~pop_100000+ med_inc_10000 + sunab(month_treat, time_to_treat, ref.c = (0), ref.p = c(-1,-1000)) | agency + date,data = all_transit_census)
summary(sunab_did)
aggregate(sunab_did, "cohort")
aggregate(sunab_did, "ATT")


#COEFFIENT PLOTS FOR TIME AND COHORT GO BELOW HERE
#Plotting
pdf("images/coef_plot_reg.pdf")
iplot(sunab_did, 
      drop     = "[[:digit:]]{2}", 
      xlab     = "Time to Treatment",
      main     = "",
      ref.line      = 0
)
dev.off()



agg_coef_cohort <- as.data.frame(rbind(aggregate(sunab_did, "cohort"),
                                       aggregate(sunab_did, "ATT")))
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
ggsave("images/catt_plot.pdf")

#PLOT FOR VOLATILITY OF RIDERSHIP



#PLACEBO TESTING
all_transit_census$vrh_pop <- all_transit_census$vrh/all_transit_census$pop
base_twfe_placebo <- feols(vrh ~pop_100000 + med_inc_10000 + i(time_to_treat, treated,
                                                                 ref = c(-1, -1000)) | agency + date, all_transit_census)
sunab_placebo<- feols(vrh ~pop_100000+ med_inc_10000 + sunab(month_treat, time_to_treat, ref.c = (1000), ref.p = c(-1,-1000)) | agency + date,data = all_transit_census)
# summary(sunab_placebo)
# aggregate(sunab_placebo, "cohort")
# aggregate(sunab_placebo, "ATT")

pdf("images/coef_plot_placebo.pdf")
iplot(base_twfe_placebo, 
      drop     = "[[:digit:]]{2}", 
      ylim     = c(-5000,10000),
      xlab     = "Time to Treatment",
      ref.line      = 0
)


# The aggregate effect for each period
agg_coef = aggregate(sunab_placebo, "period")[c(39:56),]
x = c(-9:-2, 0:9) + .35
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)

legend("bottomleft", col = c(1, 4), pch = c(20, 17),
       legend = c("TWFE", "IW Estimates"), bty = "n")
dev.off()
