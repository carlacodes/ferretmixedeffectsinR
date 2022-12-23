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

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)


dfuse <- read.csv("dfuse.csv") 
dfcatuse <- read.csv("dfcat_use.csv")
dfcattest <- read.csv('mergedtrainedandnaive.csv')

df <- read.csv("df.csv")
dfcat <- read.csv("dfcat.csv")
#run an anova to figure out the best variables to used for the random effects
df$side=factor(df$side)
df$pitchoftarg=factor(df$pitchoftarg)
df$stepval=factor(df$stepval)
df$AM=factor(df$AM)
df$ferret=factor(df$ferret)
df$pastcatchtrial=factor(df$pastcatchtrial)
df$pastcorrectresp = factor(df$pastcorrectresp)

##fit individual model to each animal
#look at reaction time mixed effects model in humans or any other types of studies 
nullmodel1 <- lmer( realRelReleaseTimes ~ 1 + (1|ferret), data = df, REML=FALSE)
nullmodel2 <- lmer( realRelReleaseTimes ~ 1 + (1 + pastcorrectresp |ferret), data = df, REML=FALSE)
nullmodel22 <- lmer( realRelReleaseTimes ~ 1 + (1 + pastcorrectresp |ferret)+(1 + trialNum |ferret), data = df, REML=FALSE)

nullmodel3 <- lmer( realRelReleaseTimes ~ 1 +(1 +pastcorrectresp+pastcatchtrial |ferret), data = df, REML=FALSE)

nullmodel4 <- lmer( realRelReleaseTimes ~ 1 +(0 +pastcorrectresp |ferret), data = df, REML=FALSE)

nullmodel5 <- lmer( realRelReleaseTimes ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret), data = df, REML=FALSE)
nullmodel6 <- lmer( realRelReleaseTimes ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret), data = df, REML=FALSE)

nullmodel7 <- lmer( realRelReleaseTimes ~ 1 + (1 + talker+timeToTarget*side |ferret), data = df, REML=FALSE)
nullmodel8 <- lmer( realRelReleaseTimes ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret), data = df, REML=FALSE)

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6, nullmodel7, nullmodel8, nullmodel22)





modelreg_reduc1 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE, )#control = lmerControl(optimizer ="Nelder_Mead")

modelreg_reduc2 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE,)

modelreg_reduc3 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE,)

modelreg_reduc4 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE, )

modelreg_reduc5 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)

modelreg_reduc55 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+talker*stepval+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = TRUE)

modelreg_reduc6 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+AM+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)
# talker*(pitchoftarg)+side + talker*stepval+timeToTarget
modelreg_reduc7 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+talker*pitchoftarg+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)

modelreg_reduc8 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+trialNum+pastcorrectresp+pastcatchtrial+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)
modelreg_reduc9 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval*talker+trialNum+(0 +pastcorrectresp |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)


anova(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3, modelreg_reduc4, modelreg_reduc5, modelreg_reduc55,modelreg_reduc6, modelreg_reduc7, modelreg_reduc8, modelreg_reduc9)
coeff=r2(modelreg_reduc55)

oneferret=subset(df, ferret == 1)
zoladata=subset(df, ferret==0)
tinadata=subset(df, ferret==2)
macdata=subset(df, ferret==3)
cruellact=oneferret['realRelReleaseTimes']

cruellapred=predict(modelreg_reduc55, oneferret, type='response')
zolapred=predict(modelreg_reduc55, zoladata, type='response')
tinapred=predict(modelreg_reduc55, tinadata, type='response')
macpred=predict(modelreg_reduc55, macdata, type='response')


plot(as.numeric(unlist(oneferret['realRelReleaseTimes'])), cruellapred, main="Cruella actual vs. predicted lick release times",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(zoladata['realRelReleaseTimes'])), zolapred, main="Zola actual vs. predicted lick release times",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(tinadata['realRelReleaseTimes'])), tinapred, main="Tina actual vs. predicted lick release times",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(macdata['realRelReleaseTimes'])), macpred, main="Mac actual vs. predicted lick release times",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)
max(df$pastcatchtrial)

plot_model(modelreg_reduc55)

