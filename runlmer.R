## load lme4, JWileymisc, and multilevelTools packages
## (i.e., "open the 'apps' ") 
library(lme4)
library(lmerTest)
library(performance)
library (texreg) #Helps us make tables of the mixed models
library (afex) # Easy ANOVA package to compare model fits
library (plyr) # Data manipulator package
library (ggplot2)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)

dfuse <- read.csv("dfuse.csv") 
dfcatuse <- read.csv("dfcat_use.csv")

df <- read.csv("df.csv")
dfcat <- read.csv("dfcat.csv")
#run an anova to figure out the best variables to used for the random effects
df$side=factor(df$side)
df$pitchoftarg=factor(df$pitchoftarg)
df$stepval=factor(df$stepval)
df$AM=factor(df$AM)
df$ferret=factor(df$ferret)



nullmodel1 <- lmer( realRelReleaseTimes ~ 1 + (1|ferret), data = df, REML=FALSE)
nullmodel2 <- lmer( realRelReleaseTimes ~ 1 + (1 + timeToTarget |ferret), data = df, REML=FALSE)
nullmodel3 <- lmer( realRelReleaseTimes ~ 1 + (1 + talker+timeToTarget |ferret), data = df, REML=FALSE)

nullmodel4 <- lmer( realRelReleaseTimes ~ 1 + (1 + talker+side+timeToTarget |ferret), data = df, REML=FALSE)
nullmodel5 <- lmer( realRelReleaseTimes ~ 1 + (1 + talker+side+timeToTarge+AM |ferret), data = df, REML=FALSE)
nullmodel6 <- lmer( realRelReleaseTimes ~ 1 + (1 + side+ timeToTarget+AM+DaysSinceStart |ferret), data = df, REML=FALSE)

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6)





modelreg_reduc1 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+ (1 + talker+side+timeToTarget|ferret),
  data=df, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

modelreg_reduc2 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+ side+(1 + talker+side+timeToTarget |ferret),
  data=df, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

modelreg_reduc3 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+(1 + talker+side+timeToTarget |ferret),
  data=df, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

modelreg_reduc4 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+(1 + talker+side+timeToTarget |ferret),
  data=df, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

modelreg_reduc5 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval*talker+side*timeToTarget+(1 + talker+side+timeToTarget |ferret),
  data=df, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3, modelreg_reduc4, modelreg_reduc5)
# m1=lmer(realRelReleaseTimes ~ pizza + (1 + pizza + time |subject), data=pizzadata, REML = FALSE)
# m2= lmer(mood ~ pizza + time + (1 + pizza + time |subject), data=pizzadata, REML = FALSE)
# 
r2coeff <-r2_nakagawa(modelreg_reduc2)
