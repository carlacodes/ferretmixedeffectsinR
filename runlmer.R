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
nullmodel2 <- lmer( realRelReleaseTimes ~ 1 + (1 + AM |ferret), data = df, REML=FALSE)
nullmodel3 <- lmer( realRelReleaseTimes ~ 1 +(1 +talker |ferret), data = df, REML=FALSE)

nullmodel4 <- lmer( realRelReleaseTimes ~ 1 +(0 +talker |ferret), data = df, REML=FALSE)

nullmodel5 <- lmer( realRelReleaseTimes ~ 1 + (0 +side |ferret)+(0 +talker |ferret), data = df, REML=FALSE)
nullmodel6 <- lmer( realRelReleaseTimes ~ 1 + (1 +side |ferret)+(1 +talker |ferret), data = df, REML=FALSE)

nullmodel7 <- lmer( realRelReleaseTimes ~ 1 + (1 + talker+timeToTarget*side |ferret), data = df, REML=FALSE)
nullmodel8 <- lmer( realRelReleaseTimes ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret), data = df, REML=FALSE)

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6, nullmodel7, nullmodel8)





modelreg_reduc1 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE, )#control = lmerControl(optimizer ="Nelder_Mead")

modelreg_reduc2 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE,)

modelreg_reduc3 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE,)

modelreg_reduc4 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE, )

modelreg_reduc5 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)

modelreg_reduc6 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+AM+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)
# talker*(pitchoftarg)+side + talker*stepval+timeToTarget
modelreg_reduc7 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+talker*pitchoftarg+side+timeToTarget+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)

modelreg_reduc8 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)
modelreg_reduc9 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval*talker+(0 +side |ferret)+(0 +talker |ferret),
  data=df, REML = FALSE)


anova(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3, modelreg_reduc4, modelreg_reduc5, modelreg_reduc6, modelreg_reduc7, modelreg_reduc8, modelreg_reduc9)

oneferret=subset(df, ferret == 1)
zoladata=subset(df, ferret==0)
tinadata=subset(df, ferret==2)
macdata=subset(df, ferret==3)
cruellact=oneferret['realRelReleaseTimes']

cruellapred=predict(modelreg_reduc7, oneferret, type='response')
zolapred=predict(modelreg_reduc7, zoladata, type='response')
tinapred=predict(modelreg_reduc7, tinadata, type='response')
macpred=predict(modelreg_reduc7, macdata, type='response')


plot(as.numeric(unlist(oneferret['realRelReleaseTimes'])), cruellapred, main="Cruella actual versus predicted",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(zoladata['realRelReleaseTimes'])), zolapred, main="Zola actual v predicted",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(tinadata['realRelReleaseTimes'])), tinapred, main="Tina actual v predicted",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(macdata['realRelReleaseTimes'])), macpred, main="Mac actual v predicted",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

