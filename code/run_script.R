source("code/housekeeping.r") #Installing necessary packages
source("code/upt_data.r") #Downloading base NTD Data
source("code/vrh_data.r") #Downloading base VRH Data
source("code/merge_vrh_upt.r") #Merging NTD and VRH Data
source("code/treating_ntd.r") #Treating NTD Data according to list derived from chaddick institute
source("code/census_data.r") #Downloading base Census Data
source("code/merge_data.r") #Merging NTD and Census Data. The csv ntd_census.csv should be used for all analysis

source("code/summary_stats.r") #Creates summary stats (saved as combo of tex, txt, csv in tables folder)
source("code/analysis_tables.r") #Creates regression analysis and table (saved as csv)
source("code/figures.r") #Creates figures (saved as png and pdf in figures folder)

