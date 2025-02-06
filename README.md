# Robomoth

Here we start exploring and analyzing the data collected with robomoths and recording at the pioneer lights study

#### 2/5/2024

I need to build the data. to see how these look. I ran the data with two versions one with Sonobatv420 and the other with Sonobatv4.4.5. this would be a good moment to compare them, but I predict that they will be different.\
I have realized that a good way to analyze the data is to take in consideration the column X.Spp and the column X.Prob to to increase the data we have, then when cleaning the data we merge that one with the SppAccp and then we have more data.

------------------------------------------------------------------------

**2/07/2024**

I created a database for Robomoths deployed with SM4 in 2021. I want to compare the number of species and number of calls detected between Sonobat versions.

There is an error with the sonobatv4.4.5 that makes the data move and show in column where it does not belong.\
\

------------------------------------------------------------------------

07/24/2024

we changed to kpro for bat idetification software in 2024.

------------------------------------------------------------------------

07/25/2024

I started counting buzzes at sites. It seems like there's some problems with the recordings. The sound of the robo-moth masks sometimes the calls

------------------------------------------------------------------------

08/6/2024

I realized that kpro has trouble reading the time from the file name when the recording comes from a robomoth. the time sometimes is ends with a T and others not.

```         
"20220805_002311.WAV"  "20220714_004620T.WAV"
```

This creates trouble for kpro and I guess sonobat too for reading the time and displaying it correctly. I am fixing that today.

------------------------------------------------------------------------

08/27/2024

I created the object c_sumry but has the daily data for bats. I am realizing maybe doing daily is not the best idea. because we have fewer monitoring nights witht the robomoths.

look at this table with all the summarized by day. There's not much there.

``` r
table(c_sumry$sp) # do we have enough for sp...

       ANTPAL CORTOW EPTFUS EUDMAC LASCIN LASNOC MYOCAL MYOCIL MYOEVO MYOLUC MYOTHY MYOVOL MYOYUM   NOID  NOISE PARHES 
    15     55     41     75      1     82     82     53     77     69     83     24     83      7     79     87     60 
```

------------------------------------------------------------------------

8/28/2024

-   I loaded 2023 data. but needs to be cleaned and merge.

-   I realized I have data for 2021 that has been ID-ed with Kaleidoscope. I just need to get it and merge it with the rest. Currently we have 2021 with sonobat and 2022-2023 with Kaleidoscope.

-   seems like Iron03 is missing from 2023 data.

-   there is a problem with the robomoth data for 2023 it seems like there is just data for 6 sites... I wonder what is going on. I am worried all the other sites have no data...

-   ok I figure out what is the problem. Most of the robomoth data got copied into the folder

    -   Z:\PioneerLights\_2023\robomoth\_2023_all

    -   while in the folder:Z:\PioneerLights\_2023\robomoth there are some files that are not present in the robomoth_2023_all.

    ------------------------------------------------------------------------

    9/04/2024

-   I did the plots for elevation, lunar illumination and treatment. However I am feeling a little but confuse by the treatment results. The model says that light has a positive effect.

    ``` r
    summary(m1.2nb)
     Family: nbinom2  ( log )
    Formula:          n ~ trmt_bin + jday_s + I(jday_s^2) + percent_s + l.illum_s +  
        avg_wind_speed_s + avg_temperature_s + elev_mean_s + (1 |  
        site) + (1 + trmt_bin + jday_s + I(jday_s^2) | sp)
    Data: c_sumry

         AIC      BIC   logLik deviance df.resid 
      8977.8   9080.3  -4467.9   8935.8      952 

    Random effects:

    Conditional model:
     Groups Name        Variance Std.Dev. Corr              
     site   (Intercept) 0.028084 0.16758                    
     sp     (Intercept) 4.196545 2.04855                    
            trmt_bin    0.096419 0.31051   0.23             
            jday_s      0.063065 0.25113   0.14  0.24       
            I(jday_s^2) 0.007095 0.08423   0.28  0.25 -0.33 
    Number of obs: 973, groups:  site, 15; sp, 17

    Dispersion parameter for nbinom2 family (): 1.22 

    Conditional model:
                      Estimate Std. Error z value Pr(>|z|)    
    (Intercept)        3.09880    0.51222   6.050 1.45e-09 ***
    trmt_bin           0.29174    0.10078   2.895  0.00379 ** 
    jday_s            -0.02045    0.08655  -0.236  0.81321    
    I(jday_s^2)       -0.08379    0.06887  -1.217  0.22374    
    percent_s          0.02427    0.05994   0.405  0.68553    
    l.illum_s          0.19114    0.04318   4.426 9.59e-06 ***
    avg_wind_speed_s  -0.05017    0.04110  -1.221  0.22226    
    avg_temperature_s  0.08479    0.05286   1.604  0.10869    
    elev_mean_s        0.15205    0.05847   2.601  0.00931 ** 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    ```

However, when I plot the model for each species, it seems like a lot have less calls in lit areas.

![](figs/trmt_raneff_v1.png)

My first thought is that I did something wrong with the code.\
I also believe that there might be an error with the plot code. I might be predicting values wrong. I need to check this later.

------------------------------------------------------------------------

9/5/2024

-   I reworked the activity index because it was not removing lines with the same site, sp, date, so when summarized the result was the same as counting calls.

-   I ran the model using the activity index and the results are similar to what the whole calls indicate. in the global model light is not significat, but in when we plot the random effects we see the effects of light on certain sp\
    *I realized I did not include yr in the model.*\

```{r}
m2.2nb <- glmmTMB(
  activity_min ~ trmt_bin + scale(jday) + I(scale(jday) ^ 2) + scale(percent)  + scale(l.illum) +  #if fit with jady and jday^2 on the random slopes then it does not work.
    scale(avg_wind_speed) + scale(avg_temperature)  + scale(elev_mean) + moo +
    (1 | site) + (1 + trmt_bin | sp),
  data = robo.ai,
  nbinom2(link = "log")
)

> summary(m2.2nb)
 Family: nbinom2  ( log )
Formula:          activity_min ~ trmt_bin + scale(jday) + I(scale(jday)^2) + scale(percent) +      scale(l.illum) + scale(avg_wind_speed) + scale(avg_temperature) +  
    scale(elev_mean) + moo + (1 | site) + (1 + trmt_bin | sp)
Data: robo.ai

     AIC      BIC   logLik deviance df.resid 
  4361.3   4431.4  -2165.7   4331.3      777 

Random effects:

Conditional model:
 Groups Name        Variance Std.Dev. Corr 
 site   (Intercept) 0.1172   0.3424        
 sp     (Intercept) 0.7943   0.8913        
        trmt_bin    0.1080   0.3286   0.78 
Number of obs: 792, groups:  site, 15; sp, 14

Dispersion parameter for nbinom2 family (): 1.62 

Conditional model:
                       Estimate Std. Error z value Pr(>|z|)    
(Intercept)             1.86620    0.27570   6.769 1.30e-11 ***
trmt_bin                0.11754    0.14315   0.821  0.41159    
scale(jday)             0.17390    0.05907   2.944  0.00324 ** 
I(scale(jday)^2)       -0.61429    0.06481  -9.479  < 2e-16 ***
scale(percent)          0.01238    0.10502   0.118  0.90616    
scale(l.illum)         -0.04464    0.05122  -0.871  0.38354    
scale(avg_wind_speed)   0.25248    0.04700   5.372 7.77e-08 ***
scale(avg_temperature) -0.63498    0.05584 -11.371  < 2e-16 ***
scale(elev_mean)        0.09687    0.10495   0.923  0.35597    
moo                    -0.01169    0.10610  -0.110  0.91226    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

```

the plot looks like this, mostly looks like bats attacked more robomoths in lit areas.

![](figs/trmt_ai_raneff_v1.png)

------------------------------------------------------------------------

2/6/2025

We need to reanalyze this data using the sonobat buzz detector tool. I need to \
organize the 2021 data and put it all in the same folder.\

2022 I don't know the status. I ams sure it has been ran through all the buzz detctor

2023 same, it has been run but not sure if it has been vetted.

I added the creaters data. We need the moon data.
