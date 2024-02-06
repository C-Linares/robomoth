
# Explore Robomoth Data

# This script explores the data for the robomoths deployed with sm4 in 2021.



# library -----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)


# load data ---------------------------------------------------------------

robo2021_raw<-read.csv('datasets/robo2021.csv')

colSums(is.na(robo2021_raw)) # several NAs

keep <-
  c("Path",
    "Filename",
    "HiF",
    "LoF",
    "SppAccp",
    "Prob",
    "calls.sec",
    "X1st",
    "X2nd")



