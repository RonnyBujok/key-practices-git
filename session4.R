library(tidyverse)
library(pwr)
library(simr)
library(WebPower)

set.seed(1)

#playing around with hypothetical correlations
pwr.r.test(n = NULL,
           r = 0.3,
           power = 0.8,
           sig.level = 0.05,
           alternative = "two.sided")


####################### Actual Simulation #################

# Load in mock data
df <- read.csv("SimData_SoPVoc.csv")

#get the correlation from the dataset
df_corr <- cor(df$meanRT,df$produce, method=("pearson"))

# calculate the sample size needed for a two sided test (no directional hypothesis)
pwr.r.test(n = NULL,
           r = df_corr,
           power = 0.8,
           sig.level = 0.05,
           alternative = "two.sided")

# calculate the sample size needed for a one sided test
# the hypothesis is directional (i.e., negative correlation expected), therefore this makes more sense
pwr.r.test(n = NULL,
           r = df_corr,
           power = 0.8,
           sig.level = 0.05,
           alternative = "less")


# plot power curve to confirm and visualize the power at different sample sizes

res <- wp.correlation(n=seq(20,100,5),r=df_corr,alternative="less")

plot(res,type='b')
abline(h=0.8,col="blue")

#CONCLUSION: at least 77 participants needed to have enough power
