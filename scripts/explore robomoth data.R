
# 

# This script explores the data for the robomoths deployed with sm4 in 2021.
# 
# Script name: explore robomoth data
# 
# Purpose of script: wrangler data and preparing for models
# 
# Author: Carlos Linares
# 
# Date Created: 07/29/2024
# 
# Email: carlosgarcialina@u.boisestate.edu
# 
# ---------------------------
#   
#   Notes:  
# sessionInfo() at end of script


# inputs ------------------------------------------------------------------
# - datasets/robo2021.csv
# - datasets/robo2022_kpr.csv
# - 2023 stills need to be ran in kpro and vetted 

# outputs ----------------------

#
# this should be a database ready to analyze with . 

# libraries


# library -----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)
library(magrittr)


# load data and clean up ---------------------------------------------------------------

# the robo2021.csv is a product from the build_data_base_v4_2.R that is in the z drive at Barber lab. 

robo2021_raw<-read.csv('datasets/robo2021.csv',check.names = T) # this comes from sonobat need to make it kpro 
robo2022_raw<-read.csv('datasets/robo2022_kpr.csv') # product of kpro ran throuhg all data




# 2021 sonobat  -----------------------------------------------------------


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



robo2021_select<-robo2021_v1 %>% select(SppAccp,date_time,site)
names(robo2021_select)[names(robo2021_select) == "SppAccp"] <- "sp"
robo2021_select$site<-tolower(robo2021_select$site)

summary(robo2021_select)


# 2022 cleanup ------------------------------------------------------------


keep<- c(".id","INDIR","OUTDIR","FOLDER","IN.FILE","DURATION","DATE","TIME","HOUR","AUTO.ID.") # cols to keep

robo2022_raw <- robo2022_raw %>% select(all_of(keep)) # keeps variables of interest

# site
robo2022_raw$site<-str_extract(robo2022_raw$OUTDIR, "[A-Za-z]{3,4}\\d{2}")

# fix a name
robo2022_raw$site = ifelse(robo2022_raw$site %in% "Iron02","iron02", robo2022_raw$site)
unique(robo2022_raw$site) # site labels


# date 

robo2022_raw$DATE<-lubridate::ymd(robo2022_raw$DATE)
sum(is.na(robo2022_raw$DATE)) # check for NAs. 

#time

# Extract the time component and create a new column
robo2022_raw <- robo2022_raw %>%
  mutate(date_time_str = str_extract(IN.FILE, "\\d{8}_\\d{6}")) %>%
  mutate(date_time = as.POSIXct(date_time_str, format = "%Y%m%d_%H%M%S"))


# slect columns to combine
robo2022_select<-robo2022_raw %>% select(AUTO.ID.,date_time,site)
names(robo2022_select)[names(robo2022_select) == "AUTO.ID."] <- "sp"

summary(robo2022_select)

# merge datasets ------------------------------------------------------------

# here we combine the data

c_robo<- bind_rows(robo2021_select, robo2022_select)

summary(c_robo)

  
  
  
  

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









# session info ------------------------------------------------------------

# sessionInfo()
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    
# 
# time zone: America/Denver
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] magrittr_2.0.3    data.table_1.15.4 lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1     dplyr_1.1.4       purrr_1.0.2       readr_2.1.5      
# [9] tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.1     tidyverse_2.0.0  
# 
# loaded via a namespace (and not attached):
#   [1] vctrs_0.6.5       cli_3.6.2         rlang_1.1.3       stringi_1.8.4     generics_0.1.3    glue_1.7.0        colorspace_2.1-0  hms_1.1.3        
# [9] scales_1.3.0      fansi_1.0.6       grid_4.4.1        munsell_0.5.1     tzdb_0.4.0        lifecycle_1.0.4   compiler_4.4.1    fs_1.6.4         
# [17] timechange_0.3.0  pkgconfig_2.0.3   rstudioapi_0.16.0 R6_2.5.1          tidyselect_1.2.1  utf8_1.2.4        pillar_1.9.0      tools_4.4.1      
# [25] withr_3.0.0       gtable_0.3.5  