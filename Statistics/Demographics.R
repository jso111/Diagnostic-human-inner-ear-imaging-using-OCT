# Simple Medical statistics 
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(table1)



#####
# Load data and clean it
#####
dTable <- tibble(read.csv('Demographics.csv'))
dTable$Disease<-factor(dTable$Disease)
dTable$Race<-factor(dTable$Race)
dTable$Ethnicity<-factor(dTable$Ethnicity)
dTable$Gender<-factor(dTable$Gender)
str(dTable)



#summary Stats

head(dTable)


table1(~Gender + Age..years. + Race + Ethnicity | Disease, data=dTable)

