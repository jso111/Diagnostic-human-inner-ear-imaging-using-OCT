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
dTable <- tibble(read.csv('MultiEPreformat_updated.csv'))
dTable$Disease<-factor(dTable$Disease)
dTable$Patient<-factor(dTable$Patient)
dTable$Measure<-factor(dTable$Measure)
str(dTable)



# Scatter plots

ggplot(data=dTable) +
  geom_point(aes(x=Patient, y=LSCC, color=Disease))                

ggplot(data=dTable) +
  geom_point(aes(x=Patient, y=PSCC, color=Disease))  

# Box plots

ggplot(data=dTable) +
  geom_boxplot(aes(x=Patient, y=LSCC, color=Disease))

ggplot(data=dTable) +
  geom_boxplot(aes(x=Patient, y=PSCC, color=Disease))

# colorblind friendly plots

highcontrast <- c("#004488","#DDAA33","#BB5566")

ggplot(data=dTable) +
  (aes(x=Patient, y=LSCC, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("LSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.085) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data=dTable) +
  (aes(x=Patient, y=PSCC, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("PSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.085) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


###calculate variance

view(dTable)
head.matrix(dTable)


#Summary stats

dTable %>% 
  group_by(Patient) %>% 
  summarize(
    LSCC.EP = mean(LSCC, na.rm = TRUE),
    PSCC.EP = mean(PSCC, na.rm = TRUE)
  )
  
dTable %>% 
  group_by(Patient) %>% 
  get_summary_stats(type = "mean_sd")

dTable %>% 
  group_by(Disease) %>% 
  get_summary_stats(type = "mean_sd")

dTable %>% 
  group_by(Patient) %>% 
  get_summary_stats(LSCC, type = "mean_sd")
mean(0.006, 0.015, 0.023, 0.009, 0.008, 0.018, 0.025, 0.03, 0.014, 0.013, 0.013, 0.014, 0.015, 0.016, 0.011, 0.019)

dTable %>% 
  group_by(Patient) %>% 
  get_summary_stats(PSCC, type = "mean_sd")
mean(0.012, 0.014, 0.013, 0.01, 0.01, 0.009, 0.007, 0.013, 0.021, 0.016, 0.023, 0.013, 0.028, 0.009, 0.015)

#Find outliers

outliers_LSCC <- dTable %>%
  group_by(Patient) %>%
  identify_outliers(LSCC)
outliers_LSCC

outliers_PSCC <- dTable %>%
  group_by(Patient) %>%
  identify_outliers(PSCC)
outliers_PSCC

# Group by Patient and calculate variance of SCC
variance_LSCC <- dTable %>%
  group_by(Patient) %>%
  summarize(variance_LSCC = var(LSCC))
variance_LSCC

variance_PSCC <- dTable %>%
  group_by(Patient) %>%
  summarize(variance_PSCC = var(PSCC))
variance_PSCC


# Mean within Patient sample variance
Mean_variance_LSCC <- dTable %>%
  group_by(Patient) %>%
  summarize(Mean_variance_LSCC = mean(c(var(LSCC))))
Mean_variance_LSCC

# Within Patient sample standard deviation
SD <- dTable %>%
  group_by(Patient) %>%
  summarize(SD = sqrt(c(var(LSCC))))
SD

# Coefficient of repeatability
RC <- dTable %>%
  group_by(Patient) %>%
  summarize(RC = 1.96 * sqrt(2) * sqrt(c(var(LSCC))))
RC


# Homogeneity of variances test

bartlett.test(LSCC ~ Patient, data = dTable)
bartlett.test(PSCC ~ Patient, data = dTable)

fligner.test(LSCC ~ Patient, data = dTable)
fligner.test(PSCC ~ Patient, data = dTable)


#repeated measures ANOVA

model_lm <- lm(LSCC ~ Measure, data = dTable)
model_aov <- aov(LSCC ~ Measure, data = dTable)
summary(model_lm)
summary(model_aov)

model_lm <- lm(PSCC ~ Measure, data = dTable)
model_aov <- aov(PSCC ~ Measure, data = dTable)
summary(model_lm)
summary(model_aov)


res.aov <- anova_test(data = dTable, dv = LSCC, wid = Patient, within = Measure)
get_anova_table(res.aov)

res.aov <- anova_test(data = dTable, dv = PSCC, wid = Patient, within = Measure)
get_anova_table(res.aov)


#intraclass correlation coefficient

head.matrix(dTable)

summary(dTable)

summary(dTable$LSCC)

dTable_no_NA <- dTable %>% 
  drop_na(LSCC)

summary(dTable_no_NA)

icc(dTable_no_NA, model = "twoway", type = "consistency", unit = "single")


dTable_no_NA <- dTable[complete.cases(dTable),]
dTable_no_NA

icc(dTable_no_NA$LSCC, model = "twoway", type = "consistency", unit = "single")
  
icc(dTable$PSCC, model = "twoway", type = "consistency", unit = "single")



dTable %>% 
  filter(!is.na(LSCC)) %>%
  icc(dTable$LSCC, model = "twoway", type = "consistency", unit = "single")


  icc(dTable$LSCC, model = "twoway", type = "consistency", unit = "single")


  
  

icc(dTable$LSCC, model = "twoway", type = "consistency", unit = "single")

icc(dTable$PSCC, model = "twoway", type = "consistency", unit = "single")



library(psych)

ICC(dTable$LSCC,missing=TRUE,alpha=.05)

