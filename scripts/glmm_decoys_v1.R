
# ---------------------------
##
## Script name: glmm_decoys_v1
##
## Purpose of script: running modles for the robomoth and speakers data collected in the pioneers light project. 
##
## Author: Carlos Linares
##
## Date Created: 08/27/2024
##
## Email: carlosgarcialina@u.boisestate.edu
##
## ---------------------------
##
## Notes: need to annotate what script produced each data set imputed. 
##   
##
## ---------------------------
## # inputs ------------------------------------------------------------------
#   data_for_analysis/prep_for_glmm/bm.csv
#   data_for_analysis/Bat_trait.csv
# outputs ----------------------

# 

# this should be a database ready to analyze with the glmm_v1 script. 



# libraries  --------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lme4)
library(sjPlot)
library(ggeffects)
library(car)
library(glmmTMB)
library(corrplot)


# data 

c_sumry<-read.csv('datasets/for_glmm/c_sumry.csv')

# correlation 
numeric_cols<- sapply(c_sumry, is.numeric) # separate all the num col
cor1<-c_sumry[,numeric_cols] #keeps just the numeric

c1<- cor(cor1,use="pairwise.complete.obs")
corrplot(c1, order= 'AOE')

# make jday 

c_sumry$jday<-yday(c_sumry$noche)



# List of variable names to be scaled
variables_to_scale <- c(
  "elev_mean",
  "percent",
  "jday",
  "avg_wind_speed",
  "avg_temperature"
)
# "l.illum", # need to add it to the data.
# "phase",
# "fraction"


# Loop over each variable, scale it, and assign it back to the data frame with a new name
for (var in variables_to_scale) {
  c_sumry[[paste0(var, "_s")]] <- scale(c_sumry[[var]], center = TRUE, scale = TRUE)
}
