all_transit <- read_csv("data/transit_treated_new_start.csv") %>%
  filter(year < 2020)

#base TWFE
base_twfe <- feols(log_ridership ~ i(time_to_treat, treated,
                            ref = c(-1, -1000)) | agency + date, all_transit)
sa_did <- feols(log_ridership ~ sunab(month_treat, time_to_treat, ref.c = (1000), ref.p = c(-1,-1000))| 
                  agency + date, all_transit) #SA DiD

att_base = aggregate(base_twfe, agg = "(tr.*ed)")
att_sa = aggregate(sa_did, agg = "cohort")


pdf("images/coef.pdf")
iplot(base_twfe, 
      drop     = "[[:digit:]]{2}", 
      ylim     = c(-0.25, 0.2),
      main     = "Effect of Voucher Programs on Ridership",
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
       legend = c("TWFE", "Sun & Abraham"), bty = "n")
dev.off()
