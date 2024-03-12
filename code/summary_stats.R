#Rankings sum stats
rankings <- read.csv("data/transit_rankings.csv")
latex_table <- stargazer(rankings, title = "Summary Statistics", type = "latex")

#Ridership sum stats
ridership <- read.csv("data/transit_data.csv")
st(ridership, vars = c("ridership", "x3_mode"), 
   out = "latex")
   