
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
## hash-2.2.6 provided by Decision Patterns
dfuse <- read.csv("dfuse.csv") 
dfcatuse <- read.csv("dfcat_use.csv")
modelreg_reduc <- list()
df <- read.csv("df.csv")

dfcat <- read.csv("dfcat.csv")
storemod <- list()
r2coeff <- list()
df$side=factor(df$side)
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
  #pitchoftarg*talker*stepval*side*timeToTarget*trialNum+pastcorrectresp*pastcatchtrial,
  #pitchoftarg*talker*stepval*side*timeToTarget*trialNum+pastcorrectresp*pastcatchtrial
  df_animal <- subset(df, ferret == i)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
 # y ~ (.)^2
  modelreg_reduc5 <- glm(
    realRelReleaseTimes  ~ pitchoftarg*talker*stepval*side*timeToTarget*trialNum+pastcorrectresp*pastcatchtrial,
    data=df_animal, family = 'gaussian')
  storemod[i+1] = modelreg_reduc5
  r2coeff[i+1] = r2(modelreg_reduc5)
  print(r2(modelreg_reduc5))
  
}