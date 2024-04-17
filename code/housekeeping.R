# ---------------------------------------------------------------------------------------------
# File: housekeeping.R
# By: Charlie Berman
# Date: November
# Description: This file installs and loads packages. It also defines the file paths. Run it 
# before running any other files. In fact, include it all files you run.
# ---------------------------------------------------------------------------------------------

# Include any other folders you may want

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,stringr,jsonlite,tidyverse, ggplot2, dplyr, janitor,lubridate,rvest,data.table, here,tidycensus,polite,stargazer,sf,leaflet,AER,lattice,fixest,haven,
               gridExtra,sf,tmap,tigris, parallel,rsample,rpart,rpart.plot,ipred,caret,boot,utils,vtable,broom,xtable,schoRsch,
               readxl, tidysynth, Hmisc, tidysynth,wesanderson,units, future.apply, purrr, skimr,pander, kableExtra,dotwhisker,plm,
               lme4, estimatr, did2s, did)

data <- '../data/'
documentation <- '../documentation/'
code <- '../code/'
output <- '../output/'
writing <- '../writing/'
presentation <- '../presentations/'
lit <- '../literature/'

