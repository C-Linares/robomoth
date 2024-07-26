# ---------------------------
##
## Script name:  Manually vetted calls  
##
## Purpose of script: count the number of feeding buzzes in a random sample of the robomoth recordings to to see if there is more buzzes at lit areas. 
##
##
## Author: Carlos Linares,
##
## Date Created: 07/24/2024
##
## Email: carlosgarcialina@u.boisestate.edu
##
## ---------------------------
##
## Notes: need to annotate what script produced each data set we load 
##   
##
## ---------------------------
## 


# libraries  --------------------------------------------------------------

library(tidyverse)


# data --------------------------------------------------------------------

# list wavs for 2021

setwd('Z:/') #you have to set the wd() to the place where the files are stored


patt<- "Z:/PioneerLights_2022/part2/robomoth/iron02/"


#  list wavs

myfiles <-fs::dir_ls(path = patt, recurse = T, glob = "*.WAV", ) 



set.seed(123)
sam_iron01<-print(sample(myfiles, replace = F,size = 4))
sam_iron02<-print(sample(myfiles, replace = F,size = 4))



# save image

# save.image(file = "buzz.RData")