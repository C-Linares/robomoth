
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
  "avg_temperature",
  "l.illum"
)


# Loop over each variable, scale it, and assign it back to the data frame with a new name
for (var in variables_to_scale) {
  c_sumry[[paste0(var, "_s")]] <- scale(c_sumry[[var]], center = TRUE, scale = TRUE)
}

# treatment 

litsites<-c("iron01","iron03","iron05","long01","long03")


c_sumry$treatmt<-ifelse(c_sumry$site %in% litsites , "lit", "dark") # this makes a treatment variable.

c_sumry$trmt_bin<- ifelse(c_sumry$treatmt== "lit", 1, -1)

summary(c_sumry)

#cattle

cattle <- c( # sites with cattle
  "long01",
  "long02",
  "long03" ,
  "long04",
  "long05",
  "vizc01",
  "vizc03" ,
  "vizc04",
  "vizc02"
)

c_sumry$moo<- ifelse(c_sumry$site %in% cattle, 1, -1)


# explore data


# Plot the distribution of the count data
ggplot(c_sumry, aes(x = n)) +
  geom_histogram(binwidth = 40, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Bat Calls", x = "Number of Calls", y = "Frequency")

ggplot(c_sumry, aes(x=jday, y=n, col=treatmt))+
  geom_point()+
  facet_wrap(~site)+
  theme_blank()+
  scale_color_manual(values = c("#0033A0", "#D64309"))+
  labs(title = "Bat acoustic activity 2021-2023",
       x = "Julian Day",
       y = "n calls",
       color = "Treatment")

ggplot(c_sumry, aes(x=jday, y=n, col=treatmt))+
  geom_point()+
  facet_wrap(~sp, scales = "free_y")+
  theme_blank()+
  scale_color_manual(values = c("#0033A0", "#D64309"))+
  labs(title = "Bat acoustic activity 2021-2023",
       x = "Julian Day",
       y = "n calls",
       color = "Treatment")



# modeling ----------------------------------------------------------------


m1.1 <- glmmTMB(
  n ~ trmt_bin + jday_s + I(jday_s ^ 2) + percent_s  + l.illum_s +  # less complex model 
    avg_wind_speed_s + avg_temperature_s  + elev_mean_s + moo+
    (1 |site) + (1 | sp),
  data = c_sumry,
  genpois(link = "log"))
  summary(m1.1)
  
  residual_deviance <- deviance(m1.1)
  residual_df <- df.residual(m1.1)
  
  # Calculate c-hat using residual deviance
  c_hat_deviance <- residual_deviance / residual_df
  print(c_hat_deviance)
  


m1.1nb <- glmmTMB(
  n ~ trmt_bin + jday_s + I(jday_s ^ 2) + percent_s  + l.illum_s +  # less complex model 
    avg_wind_speed_s + avg_temperature_s  + elev_mean_s + moo+
    (1 |site) + (1 | sp),
  data = c_sumry,
  nbinom2(link = "log"))
summary(m1.1nb)


m1.2nb <- glmmTMB(
  n ~ trmt_bin + jday_s + I(jday_s ^ 2) + percent_s  + l.illum_s +
    avg_wind_speed_s + avg_temperature_s  + elev_mean_s +
    (1 |site) + (1 + trmt_bin + jday_s + I(jday_s ^ 2) | sp),
  data = c_sumry,   nbinom2(link = "log")
)


summary(m1.2nb)

# Calculate residual deviance and residual degrees of freedom
residual_deviance <- deviance(m1.2nb)
residual_df <- df.residual(m1.2nb)

# Calculate c-hat using residual deviance
c_hat_deviance <- residual_deviance / residual_df
print(c_hat_deviance)

AIC(m1.1nb,m1.2nb,m1.1)




# save env ----------------------------------------------------------------

save.image(file = "wenv/glmm_decoys_v1.RData")
load('wenv/glmm_decoys_v1.RData')
