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
