# Simple Medical statistics 
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)

#####
# Load data and clean it
#####
dTable <- tibble(read.csv('EPaudioData_updated.csv'))
dTable$Disease<-factor(dTable$Disease)
str(dTable)

# simple box plots
ggplot(data=dTable) +
  geom_boxplot(aes(x=Disease, y=LSCC_EP))

ggplot(data=dTable) +
  geom_boxplot(aes(x=Disease, y=PSCC_EP))

#PTA vs. LSCC EP correlation
ggplot(data=dTable) +
  geom_point(aes(x=PTA.BC, y=LSCC_EP, color=Disease))

fit<-lm(LSCC_EP ~ PTA.BC, data=dTable)
summary(fit)

#PTA vs. PSCC EP correlation
ggplot(data=dTable) +
  geom_point(aes(x=PTA.BC, y=PSCC_EP, color=Disease))

fit<-lm(PSCC_EP ~ PTA.BC, data=dTable)
summary(fit)

#PSCC EP vs LSCC EP correlation
ggplot(data=dTable) +
  geom_point(aes(x=LSCC_EP, y=PSCC_EP, color=Disease))

fit<-lm(LSCC_EP ~ PSCC_EP, data=dTable)
summary(fit)


# t-test LSCC & PSCC EP by disease (unequal variance)
x<-dTable$LSCC_EP[dTable$Disease=='Control']
y<-dTable$LSCC_EP[dTable$Disease=='MD']
t.test(x,y)

x<-dTable$LSCC_EP[dTable$Disease=='Control']
y<-dTable$LSCC_EP[dTable$Disease=='VS']
t.test(x,y)

x<-dTable$PSCC_EP[dTable$Disease=='Control']
y<-dTable$PSCC_EP[dTable$Disease=='MD']
t.test(x,y)

x<-dTable$PSCC_EP[dTable$Disease=='Control']
y<-dTable$PSCC_EP[dTable$Disease=='VS']
t.test(x,y)

# t-test LSCC & PSCC EP by disease (equal variance)
x<-dTable$LSCC_EP[dTable$Disease=='Control']
y<-dTable$LSCC_EP[dTable$Disease=='MD']
t.test(x,y, var.equal=TRUE)

x<-dTable$LSCC_EP[dTable$Disease=='Control']
y<-dTable$LSCC_EP[dTable$Disease=='VS']
t.test(x,y, var.equal=TRUE)

x<-dTable$PSCC_EP[dTable$Disease=='Control']
y<-dTable$PSCC_EP[dTable$Disease=='MD']
t.test(x,y, var.equal=TRUE)

x<-dTable$PSCC_EP[dTable$Disease=='Control']
y<-dTable$PSCC_EP[dTable$Disease=='VS']
t.test(x,y, var.equal=TRUE)


# colorblind friendly plots

highcontrast <- c("#004488","#DDAA33","#BB5566")

ggplot(data=dTable) +
  geom_point(size=6,aes(x=LSCC_EP, y=PSCC_EP, color=Disease, shape=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_smooth(aes(x=LSCC_EP, y=PSCC_EP), color="black", method=lm, se=FALSE) +
  ylab("PSCC E/P Ratio") +
  xlab("LSCC E/P Ratio")+
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  xlim(0.02, 0.08) +
  ylim(0.02, 0.08) 

ggplot(data=dTable) +
  (aes(x=Disease, y=PSCC_EP, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("PSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.08)

ggplot(data=dTable) +
  (aes(x=Disease, y=LSCC_EP, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("LSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.08)

ggplot(data=dTable) +
  geom_point(size=6,aes(x=PTA.BC, y=LSCC_EP, color=Disease, shape=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_smooth(aes(x=PTA.BC, y=LSCC_EP), color="black", method=lm, se=FALSE) +
  ylab("LSCC E/P Ratio") +
  xlab("Pure Tone Average (dB HL)")+
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  xlim(0, 80) +
  ylim(0.02, 0.08)

ggplot(data=dTable) +
  geom_point(size=6,aes(x=PTA.BC, y=PSCC_EP, color=Disease, shape=Disease)) +
  scale_colour_manual(values=highcontrast) + 
  geom_smooth(aes(x=PTA.BC, y=PSCC_EP), color="black", method=lm, se=FALSE) +
  ylab("PSCC E/P Ratio") +
  xlab("Pure Tone Average (dB HL)")+
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  xlim(0, 80) +
  ylim(0.02, 0.08) 


####
##summary stats

summary <- dTable %>% 
  group_by(Disease) %>% 
  get_summary_stats(type = "mean_sd")
summary

