#---------------------- Header -----------------------------####
# date:         01.04.2021
# author:       Ronny Bujok (Ronny.Bujok@mpi.nl)
# filename:     session4.R
# description:  This script determines the sample sizes needed for 
#               the analysis of a one-sided Pearson correlation (negative correlation),
#               given a predetermined effect size, based on a simulated dataset, 
#               and a target power of 0.8
# project:      Key Practices Course
#------------------------------------------------------------##


#---------------------- Change log -------------------------####

#--------------- Change #1 ----------------------#
# Modified on 06.04.2021:
# change two sided power analysis to one sided analysis (i.e., negative correlation expected)
#------------------------------------------------#


# --------------------- library declaration --------------------####
library(tidyverse)
library(pwr)
library(simr)
library(WebPower)

# --------------------- playing around ----------------------####
set.seed(1)
pwr.r.test(n = NULL,
           r = 0.3,
           power = 0.8,
           sig.level = 0.05,
           alternative = "two.sided")


#---------------------- Actual Simulation -------------------####


SimData <- read.csv("SimData_SoPVoc.csv")
RT_produce_correlation <- cor(SimData$meanRT,df$produce, method=("pearson"))

# ------------------------- Change #1 --------------------------#
# pwr.r.test(n = NULL,
#            r = df_corr,
#            power = 0.8,
#            sig.level = 0.05,
#            alternative = "two.sided")

pwr.r.test(n = NULL,
           r = df_corr,
           power = 0.8,
           sig.level = 0.05,
           alternative = "less")

#---------------------------------------------------------------#

# --------------------- plot power curve ------------------####

res <- wp.correlation(n=seq(20,100,5),r=df_corr,alternative="less")

plot(res,type='b')
abline(h=0.8,col="blue")

#CONCLUSION: at least 77 participants needed to have enough power
