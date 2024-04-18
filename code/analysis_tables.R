all_transit_census <- read_csv("data/clean/ntd_census.csv") 


#Base regression (not used in paper)
simple_did <- feols(ridership_pop ~ pop_100000 + med_inc_10000 + treated:post| agency + date,data = all_transit_census)
base_twfe <- feols(ridership_pop ~pop_100000 + med_inc_10000 + i(time_to_treat, treated,
                                     ref = c(-1, -1000)) | agency + date, all_transit_census) #For coef
summary(simple_did)

#Sun Abraham Regression
sunab_did <- feols(ridership_pop ~pop_100000+ med_inc_10000 + sunab(month_treat, time_to_treat, ref.c = (0), ref.p = c(-1,-1000)) | agency + date,data = all_transit_census)
summary(sunab_did)
#Cohort and ATT used in Table 5 and Figure 5
aggregate(sunab_did, "cohort")
aggregate(sunab_did, "ATT")

reg_output <- as.data.frame(aggregate(sunab_did, "cohort"))
reg_output <- rbind(reg_output, aggregate(sunab_did, "ATT"))
write.csv(reg_output, "tables/reg_output.csv")
