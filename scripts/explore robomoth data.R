
# Explore Robomoth Data

# This script explores the data for the robomoths deployed with sm4 in 2021.



# library -----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)
library(magrittr)


# load data and clean up ---------------------------------------------------------------

# the robo2021.csv is a product from the build_data_base_v4_2.R that is in the z drive at Barber lab. 

robo2021_raw<-read.csv('datasets/robo2021.csv',check.names = T)

colSums(is.na(robo2021_raw)) # several NAs


# time
 
robo2021_raw <- robo2021_raw %>%
mutate(date_time = ymd_hms(str_extract(Filename, "\\d{8}_\\d{6}"), tz = "America/Denver"))


#site

robo2021_raw$site<-str_extract(robo2021_raw$Filename, "^[A-Za-z]{3,4}\\d{2}")

# Count number of NA values in the column
num_na <- sum(is.na(robo2021_raw[["site"]]))

# Identify rows with NA values in the column
rows_with_na <- which(is.na(robo2021_raw[["site"]]))

# remove NAs 
# this NA could be due to errors while reading the files.

robo2021_v1 <- robo2021_raw[!is.na(robo2021_raw$site), ] 



# explore -----------------------------------------------------------------


