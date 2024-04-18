
#SETTING UP THE DATASET
all_transit <- read_csv("data/transit_data.csv") %>%
  filter(year < 2020 & date >= "2016-01-01") 
# Remove duplicate observations based on date, agency, and ridership
unique_indices <- !duplicated(all_transit[, c("date", "agency", "ridership")])
#Removing expired programs
all_transit <- all_transit[unique_indices, ] %>%
  filter(uza_name != "Austin, TX" | 
         uza_name != "Philadelphia, PA" |
         uza_name != "Phoenix, AZ")
treated_agencies <- c("Livermore / Amador Valley Transit Authority",
                      "City of Charlotte North Carolina" ,
                      "Greater Dayton Regional Transit Authority",
                      "Pinellas Suncoast Transit Authority",
                      "Orange County Transportation Authority",
                      "Pierce County Transportation Benefit Area Authority",
                      "Solano County Transit",
                      "City of Detroit",
                      "Research Triangle Regional Public Transportation Authority")
control_agencies <- all_transit %>%
  filter(!(agency %in% treated_agencies)) %>%
  select(agency) %>%
  unique

all_transit$treated <- ifelse(all_transit$agency %in% treated_agencies, 1, 0)

#Should probably use case_when instead of ifelse
all_transit$post <- ifelse(all_transit$agency == "Livermore / Amador Valley Transit Authority" & all_transit$date >= "2016-09-01", 1,
                       ifelse(all_transit$agency == "City of Charlotte North Carolina" & all_transit$date >= "2018-04-01", 1,
                       ifelse(all_transit$agency == "Greater Dayton Regional Transit Authority" & all_transit$date >= "2017-06-01", 1,
                       ifelse(all_transit$agency == "Pinellas Suncoast Transit Authority" & all_transit$date >= "2016-02-01", 1,
                       ifelse(all_transit$agency == "Orange County Transportation Authority" & all_transit$date >= "2016-10-01", 1,
                       ifelse(all_transit$agency == "Pierce County Transportation Benefit Area Authority" & all_transit$date >= "2018-05-01", 1,
                       ifelse(all_transit$agency == "Solano County Transit" & all_transit$date >= "2017-05-01", 1,
                       ifelse(all_transit$agency == "City of Detroit" & all_transit$date >= "2018-05-01", 1,
                       ifelse(all_transit$agency == "Research Triangle Regional Public Transportation Authority"
                              & all_transit$date >= "2019-12-01",1,0)))))))))


all_transit$log_ridership <- log(all_transit$ridership)

#Creating month number and months to legalization
month_num <- rep(1:length(unique(all_transit$date)))
months <- unique(all_transit$date)
month_counter <- data.frame(month_num, months) %>%
  rename(date = months)
all_transit <- merge(all_transit, month_counter, by = "date")

date_treat <- all_transit %>%
  filter(treated == 1 & post == 1) %>%
  select(agency, month_num) %>%
  rename(month_treat = month_num) %>%
  distinct(by=agency, .keep_all = T) %>%
  select(-by)

treatment_times <- date_treat$month_treated #For treatment times

date_default <- data.frame(control_agencies, rep(0, length(control_agencies))) %>% 
  rename(month_treat = rep.0..length.control_agencies..)
date_treat <- rbind(date_treat, date_default)

all_transit <- merge(all_transit, date_treat, by = "agency")

all_transit$time_to_treat <- ifelse(all_transit$month_treat > 0, all_transit$month_num - all_transit$month_treat, -1000)

write_csv(all_transit, "data/transit_treated_new_start.csv")

#Reading in census transit data
all_transit_census <- read_csv("data/clean/all_transit_census.csv")

#Reg and coef plot
base_twfe <- feols(log_ridership ~ i(time_to_treat, treated,
                                     ref = c(-1, -1000)) | agency + date, all_transit)
twfe_fe <- feols(log_ridership ~ i(time_to_treat, treated,
                                   ref = c(-1, -1000)) | agency + date + uza_name^date, all_transit)

sa_did <- feols(log_ridership ~ sunab(month_treat, time_to_treat, ref.c = (1000), ref.p = c(-1,-1000))| 
                  agency + date, all_transit) #SA DiD
sa_did_fe <- feols(log_ridership ~ sunab(month_treat, time_to_treat, ref.c = (1000), ref.p = c(-1,-1000))| 
                  agency + date + uza_name^date, all_transit) #SA DiD

att_base = aggregate(base_twfe, agg = "(tr.*ed)")
att_base_fe = aggregate(twfe_fe, agg = "(tr.*ed)")
att_sa = aggregate(sa_did, agg = "cohort")
att_sa_fe = aggregate(sa_did_fe, agg = "cohort")


pdf("images/coef_plot_no_uza_date.pdf")
iplot(base_twfe, 
      drop     = "[[:digit:]]{2}", 
      ylim     = c(-1, 0.5),
      main     = "TWFE Difference-in-Difference w/o UZA Date Interacted Fixed Effects",
      xlab     = "Time to Treatment",
      ref.line      = 0
)


# The aggregate effect for each period
agg_coef = aggregate(sa_did, "(ti.*at)::(-?[[:digit:]]+)")[c(39:56),]
x = c(-9:-2, 0:9) + .35
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)

legend("bottomleft", col = c(1, 4), pch = c(20, 17),
       legend = c("TWFE", "IW Estimates"), bty = "n")
dev.off()
#UZA_Date FE Plot
pdf("images/coef_plot_uza_date.pdf")
iplot(twfe_fe, 
      drop     = "[[:digit:]]{2}", 
      ylim     = c(-1, 0.5),
      main     = "TWFE Difference-in-Difference w/ UZA Date Interacted Fixed Effects",
      xlab     = "Time to Treatment",
      ref.line      = 0
)

# The aggregate effect for each period
agg_coef = aggregate(sa_did_fe, "(ti.*at)::(-?[[:digit:]]+)")[c(39:56),]
x = c(-9:-2, 0:9) + .35
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)

legend("bottomleft", col = c(1, 4), pch = c(20, 17),
       legend = c("TWFE", "IW Estimates"), bty = "n")
dev.off()


#Cohort coef plot
agg_coef_cohort <- as.data.frame(rbind(att_sa_fe,att_base_fe))
agg_coef_cohort$group <- c("Pinellas Suncoast Transit Authority",
                           "Orange County Transportation Authority",
                           "Greater Dayton Regional Transit Authority",
                           "City of Charlotte North Carolina",
                           "Pierce County and Detroit",
                           "Research Triangle",
                           "ATT")
agg_coef_cohort$ci_low <- agg_coef_cohort$Estimate - 1.96 * agg_coef_cohort$`Std. Error`
agg_coef_cohort$ci_hi <- agg_coef_cohort$Estimate + 1.96 * agg_coef_cohort$`Std. Error`

catt_plot <- ggplot(agg_coef_cohort, aes(x=Estimate, y=group)) + 
  geom_point(col = 4) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_hi), height = 0.1, col = 4, size = 0.75) +
  labs(y = "Cohort",
       title = "Cohort Average Treated Effect and Average Treated Effect") +
  geom_vline(xintercept = 0) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Hard lines around the plot
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    plot.background = element_rect(fill = "white"),  # Set background color to white
    panel.background = element_rect(fill = "white") 
  )
ggsave("images/catt_plot_uza_date.pdf", plot = catt_plot, width = 8, height = 6)



#Creating avg ridership plots
ridership_treated <- all_transit %>%
  filter(treated == 1) %>%
  group_by(date)

avg_ridership_control <- all_transit %>%
  filter(treated == 0) %>%
  group_by(date) %>%
  summarise(log_ridership = mean(log_ridership))



#Graphs for checking
pre_trends <- ggplot() +
  geom_line(data = avg_ridership_control, aes(x = month_num, y = log_ridership, color = "Control")) +
  geom_line(data = ridership_treated, aes(x = month_num, y = log_ridership, color = factor(agency))) +
  geom_vline(xintercept = treatment_times, linetype = "dashed") +
  labs(title = "Ridership",
       x = "Date",
       y = "Ridership (ln)",
       color = "Treatment Status") +
  theme_minimal()
pre_trends
