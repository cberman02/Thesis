pre_covid <- read_csv("data/pre_covid.csv")
pre_covid$treated <- ifelse(pre_covid$agency %in%
                                  c("Pinellas Suncoast Transit Authority",
                                    "Livermore / Amador Valley Transit Authority",
                                    "Research Triangle Regional Public Transportation Authority"), 0, 0)
pre_covid$treated <- ifelse(pre_covid$agency %in%
                              c("Hillsborough Area Regional Transit Authority",
                                "The Eastern Contra Costa Transit Authority",
                                "Town of Chapel Hill"), 1, 0)

did_fe_date <- feols(ridership ~ treated:time | agency + date, data = pre_covid)
did_ln_date <- feols(log(ridership) ~ treated:time | agency + date, data = pre_covid)
etable(did_fe_date, did_ln_date, tex = T)
