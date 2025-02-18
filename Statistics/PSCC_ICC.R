# Simple Medical statistics 
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(rstatix)
library(irr)

#####
# Load data and clean it
#####
dTable <- tibble(read.csv('MultiEPreformat_PSCCnoNA_updated.csv'))
dTable$Disease<-factor(dTable$Disease)
dTable$Patient<-factor(dTable$Patient)
str(dTable)

#intraclass correlation coefficient

head.matrix(dTable)

summary(dTable)

icc(dTable, model = "twoway", type = "consistency", unit = "single")

icc(dTable, model = "twoway", type = "agreement", unit = "single")

icc(dTable, model = "twoway", type = "consistency", unit = "average")

icc(dTable, model = "twoway", type = "agreement", unit = "average")

