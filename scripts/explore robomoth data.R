
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

#load kpro data




# data prep ----------------------------------------------------------------


# time
 
robo2021_raw <- robo2021_raw %>%
mutate(date_time = ymd_hms(str_extract(Filename, "\\d{8}_\\d{6}"), tz = "America/Denver"))


#site

robo2021_raw$site<-str_extract(robo2021_raw$Filename, "^[A-Za-z]{3,4}\\d{2}")
unique(robo2021_raw$site)

# Count number of NA values in the column
num_na <- sum(is.na(robo2021_raw[["site"]]))

# Identify rows with NA values in the column
rows_with_na <- which(is.na(robo2021_raw[["site"]]))

# remove NAs 
# this NA could be due to errors while reading the files.

robo2021_v1 <- robo2021_raw[!is.na(robo2021_raw$site), ] 

# hrs

robo2021_v1$hrs<- hour(robo2021_v1$date_time)


# noche
robo2021_v1$noche <-
  if_else(robo2021_v1$hrs < 9, # if it is less than 9 put the date of the previous day
          true =  (date(robo2021_v1$date_time) - ddays(1)),
          false = date(robo2021_v1$date_time))

#treatment 

litsites<-c("IRON01","IRON03","IRON05","LON01","LON03")

robo2021_v1$treatmt<-ifelse(robo2021_v1$site %in% litsites , "lit", "dark") # this makes a treatment variable.

robo2021_v1$trmt_bin<- ifelse(robo2021_v1$treatmt== "lit", 1, 0)

# combine sppaccp and ~spp into a new column. 

robo2021_v1$sppall<- paste(robo2021_v1$SppAccp,robo2021_v1$X.Spp)



# explore -----------------------------------------------------------------

# counts by species

unique(robo2021_v1$SppAccp)


# species 
robo.mat <- robo2021_v1 %>%
  group_by(site, SppAccp, ) %>% # I don't include year because it is a single year
  count(SppAccp, source, .drop = FALSE) %>%  #  we might have to include the arument .drop=false to count the NAs and the zeros
  pivot_wider(names_from =source, values_from = n) %>%
  ungroup()

robo.mat2 <- robo2021_v1 %>%
  group_by(site, SppAccp ) %>% # I don't include year because it is a single year
  count(SppAccp, source, .drop = FALSE) %>%  #  we might have to include the arument .drop=false to count the NAs and the zeros
  pivot_wider(names_from =c(source), values_from = n) %>%
  ungroup()


p1<- ggplot(robo.mat, aes(x=SppAccp,y=n))+
  geom_col()+
  # facet_grid(.~ source)+
  theme_classic()
p1
