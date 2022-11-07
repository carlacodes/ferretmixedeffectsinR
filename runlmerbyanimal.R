
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
library(hash)
library(glmnet)
library(AICcmodavg)
data(QuickStartExample)
x <- QuickStartExample$x
y <- QuickStartExample$y
## hash-2.2.6 provided by Decision Patterns
dfuse <- read.csv("dfuse.csv") 
dfcatuse <- read.csv("dfcat_use.csv")
modelreg_reduc <- list()
df <- read.csv("df.csv")

dfcat <- read.csv("dfcat.csv")
storemod <- list()
aiclist <-list()
r2coeff <- list()
anovalist <- list()
df$side=factor(df$side)
df$talker=factor(df$talker)
df$pitchoftarg=factor(df$pitchoftarg)
df$stepval=factor(df$stepval)
df$AM=factor(df$AM)
df$ferret=factor(df$ferret)
df$pastcatchtrial=factor(df$pastcatchtrial)
df$pastcorrectresp = factor(df$pastcorrectresp)
dfsmall <- df[c("ferret", "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]


for (i in 0:3) {
  print(i) #pitchoftarg+talker+stepval+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial
  # pitchoftarg*talker*stepval*side*timeToTarget*trialNum+pastcorrectresp*pastcatchtrial+pastcorrectresp+pastcatchtrial
  #pitchoftarg*talker*stepval*side*timeToTarget*trialNum+pastcorrectresp*pastcatchtrial
  df_animal <- subset(df, ferret == i)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]

  ##pitch of targ is highly correlated with talker identity
  #modelreg_reduc5<- glm(realRelReleaseTimes ~  pitchoftarg*talker*stepval*side*timeToTarget*trialNum*pastcorrectresp*pastcatchtrial*AM*DaysSinceStart, data=df_animal, family = 'gaussian')
  modelreg_reduc1<- glm(realRelReleaseTimes ~  pitchoftarg+stepval+side+timeToTarget+pastcatchtrial+pastcorrectresp+AM+DaysSinceStart, data=df_animal, family = 'Gamma')
  modelreg_reduc2 <- glm(realRelReleaseTimes ~  pitchoftarg+stepval+side+timeToTarget+pastcatchtrial+pastcorrectresp+AM, data=df_animal, family = 'Gamma')
  modelreg_reduc3 <- glm(realRelReleaseTimes ~  pitchoftarg+stepval+side+timeToTarget+pastcatchtrial+pastcorrectresp, data=df_animal, family = 'Gamma')
  
  
  storemod[i+1] = modelreg_reduc1
  r2coeff[i+1] = r2(modelreg_reduc3)
 
  models <- list(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3)
  
  #specify model names
  mod.names <- c('1', '2', '3')
  
  #calculate AIC of each model
  aiclist[i+1]=BIC(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3)
  print(BIC(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3))
  print(r2(modelreg_reduc3))
  preddata=predict(modelreg_reduc3, df_animal, type='response')
  
  
  plot(as.numeric(unlist(df_animal['realRelReleaseTimes'])), preddata, main="actual vs. predicted lick release times",
       xlab="actual ", ylab="predicted ", pch=19)
  abline(a=0, b=1)
  
}

