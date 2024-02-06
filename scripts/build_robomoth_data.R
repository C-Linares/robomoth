# =================================================
# = build database from Sonobat output files  =
# =================================================
# the objective is to build a data base with the individual files that Sonobat creates. 
# these are saved in different folders depending on the year, site, instrument. 
# 
# 
# 
# Carlos Linares  12/19/2023


# libraries ---------------
library(data.table)

# build 2021 pioneer lights data base. 

setwd("Z:/PioneerLights_2021")