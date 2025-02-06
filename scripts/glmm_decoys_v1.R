
# ---------------------------
##
## Script name: glmm_decoys_v1
##
## Purpose of script: running models for the robomoth and speakers data collected in the pioneers light project. 
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

# figures:p1- 

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

c_sumry<-read_csv('datasets/for_glmm/c_sumry.csv')

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





# plots -------------------------------------------------------------------



plot_model(m1.2nb)
plot_model(m1.2nb, "re")
plot_model(m1.2nb,"pred", terms = "trmt_bin[all]")

# fixed effect plots: l.illumination and elevation

# l.illumination ----------------------------------------------------------

#we get the obs values of l.illm
n<-100
int<-rep(1,n)
l.illum<-seq(min(c_sumry$l.illum), max(c_sumry$l.illum),length.out= n)
# we scale them
l.illum_s<-scale(l.illum)
#extract fixed effects coefficients
cint<-confint(m1.2nb)
cint[c(1,6),] # get just the moon illumination and the intercept.
# then we calculate the predictions.
predcalls<- exp(t(cint[c(1,6),]) %*% t(cbind(int,l.illum_s)))

#make new df with predictions and obs values 

l.illmdf<-data.frame(t(predcalls),l.illum_s, l.illum)
colnames(l.illmdf)<-c("lowCI", "highCI","Mean")

#plot 
p1<-ggplot(l.illmdf,aes(x=l.illum, y=Mean))+
  theme_minimal(base_size = 12)+
  ylab("bat calls")+
  xlab("lunar illumination")+
  geom_line(size=1)+
  geom_ribbon(alpha=0.3, aes(ymin= lowCI, ymax=highCI))
p1

ggsave("l.illum.tiff",plot = p1, device = "tiff", path = 'figs/')

# elevation ---------------------------------------------------------------

elev<-seq(min(c_sumry$elev_mean), max(c_sumry$elev_mean),length.out= n)
elev_s<- scale(elev)

#fixed effects 
cint[c(1,8),]

# predictions
predcalls<- exp(t(cint[c(1,8),]) %*% t(cbind(int,elev_s)))

elev.df<-data.frame(t(predcalls),elev_s, elev)
colnames(elev.df)<-c("lowCI", "highCI","Mean")

#plot 
p2<-ggplot(elev.df,aes(x=elev, y=Mean))+
  theme_minimal(base_size = 12)+
  ylab("bat calls")+
  xlab("elevation")+
  geom_line(size=1)+
  geom_ribbon(alpha=0.3, aes(ymin= lowCI, ymax=highCI))
p2

ggsave("elevation.tiff",plot = p2, device = "tiff", path = 'figs/')


# we have to make trmt_bin plots  binomial categorical random effect 

# random effects plots

sl=100
ones<-rep(1,100)
#obs.val
trmt<- c("lit", "dark") 
trmt_bin_s <- c(-1,1)

#random effects 
ran.efs <- ranef( m1.2nb )$cond$sp # get the random effects 
randint<- ran.efs[,1]
randslope<- ran.efs[,2]


cint<-confint(m1.2nb)[1:2,] # get fixed effects from the model
fixint<-cint[1,3]
fixslope<-cint[2,3]

#y = int + random.int[sp] + beta[1]treatment + random.slope[sp] * treatment
# predictions as in the formula above. 

pred<- c(exp((randint+fixint)+(randslope+fixslope)*trmt_bin_s[1]),
         exp((randint+fixint)+(randslope+fixslope)*trmt_bin_s[2]))

abunddf <- data.frame(pred, trmt, trmt_bin_s)

sp_names <- rownames(ran.efs)
sp_doubled <- rep(sp_names, each = 2)

if (length(sp_doubled) == nrow(abunddf)) {
  # Add the new column to the dataframe
  abunddf$sp <- sp_doubled
} else {
  stop("The length of sp_doubled does not match the number of rows in abunddf.")
}

# View the updated dataframe
head(abunddf)

p3<-ggplot( abunddf, aes( x = trmt, y = pred, colour = sp, group = sp, shape = sp) ) +  
  theme_classic( base_size = 12) +
  scale_color_viridis_d(direction = -1, option = "A")+
  scale_shape_manual(values = 1:17) +  # Customize shapes for each 'sp', adjust as needed
  ylab( "bat calls" ) +
  xlab( "trmt" ) +
  geom_point(size=3)+
  geom_line()
p3


ggsave(filename = "trmt_raneff_v1.png",plot = p3,device = "png", path = 'figs/' )



# jday  -------------------------------------------------------------------


sl=100
ones = rep(1,100)

# obs. values
ord.day <- seq(min(c_sumry[, "jday"]), max(c_sumry[, "jday"]), length.out = n)
# extraer el jday sqr 
ord.day.sqr<-ord.day^2
#std pred
jday.s <- scale(ord.day)
jday.sqr<- scale(ord.day.sqr)

#extract fixed coef jday
#pull out random effects at the sp level #
ran.efs <- ranef( m1.5nb )$cond$sp

#pull out fixed effects
fix.efs <- fixef( m1.5nb )$cond
#view
fix.efs

cint<-confint(m1.5nb)

rss <- ran.efs

# we correct the random effects adding the fixes effects. 

rss[, 1] <- rss[, 1] + fix.efs[1]  #adding fixed effects to each of the random effects
rss[, 2] <- rss[, 2] + fix.efs[2]
rss[, 3] <- rss[, 3] + fix.efs[3]
rss[, 4] <- rss[, 4] + fix.efs[4]

#view
rss
a<-rss[,c(1,3,4)]
t(a)                      # why we have to transpose the tables?

b<-t( cbind( ones, jday.s, jday.sqr))
b
indpred<- exp( as.matrix(a) %*% as.matrix(b) )

abunddf <- data.frame(t(indpred), jday.s, ord.day)

ggplot(abunddf, aes(x = ord.day, y = MYOLUC    )) +
  theme_classic(base_size = 17) +
  ylab("bat calls") +
  xlab("jday") +
  geom_line(size = 1.5) 


# Create the melted data for plotting# Create the melted data for plotting# Create the melted data for plotting all columns
abunddf_melted <- melt(abunddf, id.vars = "ord.day",variable.name = "sp", value.name ="predicted calls" , measure.vars = 1:14)
unique(abunddf_melted$sp)

abunddf_long<- pivot_longer(abunddf, cols= 1:14, names_to = "sp", values_to = "predicted calls")
unique(abunddf_long$sp)
head(abunddf_long)

# Create the ggplot object
p6<-ggplot(abunddf_melted, aes(x = ord.day, y = `predicted calls`, color = sp)) +
  scale_color_viridis_d()+
  # Add geom_point to plot points for each variable
  scale_shape_manual(values = 1:6)+ 
  geom_point(size = 2) +
  # Use different shapes
  # Set labels and title
  labs(title = "Abundance by Day", x = "Day", y = "bat calls") +
  # Set theme for better visuals (optional)
  theme_classic()
p6
ggplot(abunddf_long, aes(x = ord.day, y = `predicted calls`, color = sp)) +
  # Add geom_point to plot points for each variable
  geom_point(size = 1) +
  facet_wrap( ~ sp, scales = "free_y") +
  # Set labels and title
  labs(title = "Bat calls by julian day", x = "julian day", y = "bat calls") +
  # Set theme for better visuals (optional)
  theme_classic()
rm(abunddf)



##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
# acoustic activity section -----------------------------------------------

# here we ran similar models to see if the observer patters stay once we use the acoustic activity instead of the raw counts of data.

#load data

robo.ai<-read_csv('datasets/for_glmm/robo.ai.csv')

#filter out noise and NAs from the sp col

robo.ai <- robo.ai %>% filter(!sp %in% c("Noise", "NOID", "NOISE") &
                          !is.na(sp))


# correlation 
numeric_cols<- sapply(robo.ai, is.numeric) # separate all the num col
cor1<-robo.ai[,numeric_cols] #keeps just the numeric

c1<- cor(cor1,use="pairwise.complete.obs")
corrplot(c1, order= 'AOE',)
corrplot(c1, 
         method = "circle",       # Use circles for the plot
         order = 'AOE',           # Order the matrix by angle of eigenvalues
         addCoef.col = "black")  # Add correlation coefficients in black color
# make jday 

robo.ai$jday<-yday(robo.ai$noche)



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
  robo.ai[[paste0(var, "_s")]] <- scale(robo.ai[[var]], center = TRUE, scale = TRUE)
}

# treatment 

litsites<-c("iron01","iron03","iron05","long01","long03")


robo.ai$treatmt<-ifelse(robo.ai$site %in% litsites , "lit", "dark") # this makes a treatment variable.

robo.ai$trmt_bin<- ifelse(robo.ai$treatmt== "lit", 1, -1)

summary(robo.ai)

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

robo.ai$moo<- ifelse(robo.ai$site %in% cattle, 1, -1)


# explore data


# Plot the distribution of the count data
ggplot(robo.ai, aes(x = activity_min )) +
  geom_histogram(binwidth = 40, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Bat Calls", x = "Number of Calls", y = "Frequency")

ggplot(robo.ai, aes(x=jday, y=activity_min , col=treatmt))+
  geom_point()+
  facet_wrap(~site)+
  theme_blank()+
  scale_color_manual(values = c("#0033A0", "#D64309"))+
  labs(title = "Bat acoustic activity 2021-2023",
       x = "Julian Day",
       y = "n calls",
       color = "Treatment")

ggplot(robo.ai, aes(x=jday, y=activity_min, col=treatmt))+
  geom_point()+
  facet_wrap(~sp, scales = "free_y")+
  theme_blank()+
  scale_color_manual(values = c("#0033A0", "#D64309"))+
  labs(title = "Bat acoustic activity 2021-2023",
       x = "Julian Day",
       y = "n calls",
       color = "Treatment")



# modeling acoustic activity index ----------------------------------------


m2.1 <- glmmTMB(
  activity_min ~ trmt_bin + jday_s + I(jday_s ^ 2) + percent_s  + l.illum_s +  # less complex model 
    avg_wind_speed_s + avg_temperature_s  + elev_mean_s + moo+
    (1 |site) + (1 | sp),
  data = robo.ai,
  genpois(link = "log"))
summary(m2.1)

library(performance)
m2.1pform<- performance(m2.1) # the model does not converge
residuals_model <- residuals(m2.1, type = "pearson")  # You can use other types like "response" or "deviance"
fitted_values <- fitted(m2.1)

# Plot residuals vs. fitted values
plot(fitted_values, residuals_model, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")



m2.1nb <- glmmTMB(
  activity_min ~ trmt_bin + scale(jday) + I(scale(jday) ^ 2) + scale(percent)  + scale(l.illum) +  # less complex model 
    scale(avg_wind_speed) + scale(avg_temperature)  + scale(elev_mean) + moo+
    (1 |site) + (1 | sp),
  data = robo.ai,
  nbinom2(link = "log"))
summary(m2.1nb)
m2.1nbper<-performance(m2.1nb)
plot_model(m2.1nb)

m2.2nb <- glmmTMB(
  activity_min ~ trmt_bin + scale(jday) + I(scale(jday) ^ 2) + scale(percent)  + scale(l.illum) +  #if fit with jady and jday^2 on the random slopes then it does not work.
    scale(avg_wind_speed) + scale(avg_temperature)  + scale(elev_mean) + moo +
    (1 | site) + (1 + trmt_bin | sp),
  data = robo.ai,
  nbinom2(link = "log")
)


summary(m2.2nb)
plot_model(m2.2nb)

# Calculate residual deviance and residual degrees of freedom
residual_deviance <- deviance(m2.2nb)
residual_df <- df.residual(m2.2nb)

# Calculate c-hat using residual deviance
c_hat_deviance <- residual_deviance / residual_df
print(c_hat_deviance)

AIC(m2.1nb,m2.2nb)

m2.2nbper<-performance(m2.2nb)


# plots -------------------------------------------------------------------

plot_model(m2.2nb)
plot_model(m2.2nb, 're')

# treatment  random effects plot

# random effects plots ----------------------------------------------------

trmt<- c("dark", "lit") 
trmt_bin_s <- c(-1,1)
trmt_bin_s

# correcting random eff
ran.efs <- ranef( m2.2nb )$cond$sp # get the random effects 
randint<- ran.efs[,1]
randslope<- ran.efs[,2]

cint<-confint(m2.2nb)[1:2,] # get fixed effects from the model
fixint<-cint[1,3]
fixslope<-cint[2,3]


#y = int + random.int[sp] + beta[1]treatment + random.slope[sp] * treatment

pred<- c(exp((randint+fixint)+(randslope+fixslope)*trmt_bin_s[1]),
         exp((randint+fixint)+(randslope+fixslope)*trmt_bin_s[2]))


abunddf <- data.frame(pred, trmt, trmt_bin_s)

sp_names <- rownames(ran.efs)
sp_doubled <- rep(sp_names, each = 2)

# Check the length to match the number of rows in the dataframe
if (length(sp_doubled) == nrow(abunddf)) {
  # Add the new column to the dataframe
  abunddf$sp <- sp_doubled
} else {
  stop("The length of sp_doubled does not match the number of rows in abunddf.")
}

# View the updated dataframe
head(abunddf)

p5<-ggplot( abunddf, aes( x = trmt, y = pred, colour = sp, group = sp, shape = sp) ) +  
  theme_classic( base_size = 12) +
  scale_color_viridis_d(direction = 1, option = "A")+
  scale_shape_manual(values = 1:14) +  # Customize shapes for each 'sp', adjust as needed
  ylab( "bat calls" ) +
  xlab( "treatment" ) +
  geom_point(size=3)+
  geom_line()+
  # labels
  ylab("Bat calls") +
  ggtitle("Acousti index robomoth 2021-2023")
p5

p5.1 <- ggplot(abunddf, aes(x = trmt, y = pred, colour = sp, group = sp, shape = sp)) +  
  theme_classic(base_size = 16) +  # Increase base size for larger text
  scale_color_viridis_d(direction = 1, option = "A") +  # Keep the color scale
  scale_shape_manual(values = 1:14) +  # Customize shapes for each 'sp'
  ylab("Bat calls") +
  xlab("Treatment") +
  geom_point(size = 3, color = "white") +  # Set point color to white
  geom_line(color = "white") +  # Set line color to white
  ggtitle("") +
  theme(
    plot.background = element_rect(fill = "black"),  # Set plot background to black
    panel.background = element_rect(fill = "black"),  # Set panel background to black
    legend.background = element_rect(fill = "black"),  # Set legend background to black
    legend.text = element_text(color = "white"),  # Set legend text color to white
    legend.title = element_text(color = "white"),  # Set legend title color to white
    axis.text = element_text(color = "white"),  # Set axis text color to white
    axis.title = element_text(color = "white"),  # Set axis title color to white
    plot.title = element_text(color = "white"),  # Set plot title color to white
    axis.ticks = element_line(color = "white"),  # Set axis ticks color to white
    panel.grid.major = element_line(color = "black"),  # Optional: set grid lines color
    panel.grid.minor = element_line(color = "black")   # Optional: set minor grid lines color
  )

p5.1


p5 <- ggplot(abunddf, aes(x = trmt, y = pred, colour = sp, group = sp, shape = sp)) +  
  theme_classic(base_size = 12) +
  scale_color_viridis_d(direction = -1, option = "A") +
  scale_shape_manual(values = 1:14) +  # Customize shapes for each 'sp', adjust as needed
  ylab("Bat calls") +
  xlab("Year") +
  geom_point(size = 3) +
  geom_line() +
  ggtitle("Acousti index robomoth 2021-2023") +
  facet_wrap(~ sp)  # Facet by 'sp'

p5



ggsave(filename = "trmt_ai_raneff_v1.tiff",plot = p5.1, device = "tiff", path = 'figs/' , height =7 , width = 10, units = "in")


# save env ----------------------------------------------------------------

save.image(file = "wenv/glmm_decoys_v1.RData")
load('wenv/glmm_decoys_v1.RData')
