
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
storecv <- list()
storecv_correctresp <- list()

storemod_correctresp <- list()

df <- read.csv("df.csv")

dfcat <- read.csv("dfcat.csv")
storemod <- list()
aiclist <-list()
r2coeff <- list()
r2coeff_correctresp <- list()

anovalist <- list()
df$side=factor(df$side)
df$talker=factor(df$talker)
df$pitchoftarg=factor(df$pitchoftarg)
df$stepval=factor(df$stepval)
df$AM=factor(df$AM)
df$ferret=factor(df$ferret)
df$pastcatchtrial=factor(df$pastcatchtrial)
df$pastcorrectresp = factor(df$pastcorrectresp)

dfuse$side=factor(dfuse$side)
dfuse$talker=factor(dfuse$talker)
dfuse$pitchoftarg=factor(dfuse$pitchoftarg)
dfuse$stepval=factor(dfuse$stepval)
dfuse$AM=factor(dfuse$AM)
dfuse$ferret=factor(dfuse$ferret)
dfuse$pastcatchtrial=factor(dfuse$pastcatchtrial)
dfuse$pastcorrectresp = factor(dfuse$pastcorrectresp)

dfcatuse$side=factor(dfcatuse$side)
dfcatuse$talker=factor(dfcatuse$talker)
dfcatuse$pitchoftarg=factor(dfcatuse$pitchoftarg)
dfcatuse$stepval=factor(dfcatuse$stepval)
dfcatuse$AM=factor(dfcatuse$AM)
dfcatuse$ferret=factor(dfcatuse$ferret)
dfcatuse$pastcatchtrial=factor(dfcatuse$pastcatchtrial)
dfcatuse$pastcorrectresp = factor(dfcatuse$pastcorrectresp)

dfsmall <- df[c("ferret", "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

for (i in 0:3) {
  print(i) 
  df_animal <- subset(dfuse, ferret == i)
  X = subset(df_animal, select = -c(realRelReleaseTimes) )
  X = subset(X, select = -c(X) )
  X = subset(X, select = -c(ferret) )
  
  


   #X=model.matrix(X)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
  
  ##pitch of targ is highly correlated with talker identity
  #modelreg_reduc5<- glm(realRelReleaseTimes ~  pitchoftarg*talker*stepval*side*timeToTarget*trialNum*pastcorrectresp*pastcatchtrial*AM*DaysSinceStart, data=df_animal, family = 'gaussian')
  

  mod_cv <- cv.glmnet(data.matrix(X), y=df_animal$realRelReleaseTimes, family = 'gaussian', intercept = F, alpha=1)
  lambda <- mod_cv$lambda.1se
  
  modelreg_reduc1<- glmnet(X, df_animal$realRelReleaseTimes,family = 'gaussian', lambda=lambda, intercept = F, alpha = 1)
  
  
  storemod[i+1] =coef(modelreg_reduc1)
  storecv[i+1] = mod_cv
  r2coeff[i+1] =(modelreg_reduc1$dev.ratio) #dev ratio represents the pseudo r^2
  #r2 <- fit$glmnet.fit$dev.ratio[which(modelreg_reduc1$glmnet.fit$lambda == modelreg_reduc1$lambda.1se)] 
  plot(mod_cv) 
  coef(mod_cv, c(mod_cv$lambda.min,
                 mod_cv$lambda.1se))
  
  coef(mod_cv, c(mod_cv$lambda.min))
  print(paste(mod_cv$lambda.min,
              log(mod_cv$lambda.min)))
  print(paste(mod_cv$lambda.1se,
              log(mod_cv$lambda.1se)))
  
  #preddata=predict(modelreg_reduc1, data.matrix(X), type='response')
  datamat = data.matrix(X)
  predictions_train <- predict(modelreg_reduc1, newx = data.matrix(X))
  eval_results(df_animal$realRelReleaseTimes, predictions_train, df_animal)
  
  
  plot(df_animal$realRelReleaseTimes, predictions_train, main="actual vs. predicted correct relative release times",
       xlab="actual ", ylab="predicted ", pch=19)
  abline(a=0, b=1)
  
  
}

for (i in 0:3) {
  print(i) 
  df_animal <- subset(dfcatuse, ferret == i)
  X = subset(df_animal, select = -c(correctresp) )
  X = subset(X, select = -c(X) )
  X = subset(X, select = -c(ferret) )
  
  
  #X=model.matrix(X)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
  
  ##pitch of targ is highly correlated with talker identity
  #modelreg_reduc5<- glm(realRelReleaseTimes ~  pitchoftarg*talker*stepval*side*timeToTarget*trialNum*pastcorrectresp*pastcatchtrial*AM*DaysSinceStart, data=df_animal, family = 'gaussian')
  
 
  mod_cv <- cv.glmnet(data.matrix(X), y=as.factor(df_animal$correctresp), family = 'binomial', intercept = F, alpha=1)
  lambda <- mod_cv$lambda.1se
  
  modelreg_reduc1<- glmnet(X, as.factor(df_animal$correctresp),family = 'binomial', lambda=lambda, intercept = F, alpha = 1)
  
  
  storemod_correctresp[i+1] =  coef(modelreg_reduc1)
  storecv_correctresp[i+1] = mod_cv
  i<-which(mod_cv$lambda == mod_cv$lambda.1se)
  e<-mod_cv$cvm[i]
  r2<-1-e/var((df_animal$correctresp))
  r2coeff_correctresp[i+1] =modelreg_reduc1$dev.ratio #dev ratio represents the pseudo r^2
   plot(mod_cv) 
  coef(mod_cv, c(mod_cv$lambda.min,
                 mod_cv$lambda.1se))
  
  coef(mod_cv, c(mod_cv$lambda.min))
  print(paste(mod_cv$lambda.min,
              log(mod_cv$lambda.min)))
  print(paste(mod_cv$lambda.1se,
              log(mod_cv$lambda.1se)))
  
  preddata=predict(modelreg_reduc1, data.matrix(X))
  
  
  plot(as.numeric(unlist(df_animal['correctresp'])), preddata, main="actual vs. predicted correct response",
       xlab="actual ", ylab="predicted ", pch=19)

  
  
  
}



##now run both lasso for the whole dataset of animals 
  df_animal <- dfuse
  X = subset(df_animal, select = -c(realRelReleaseTimes) )
  X = subset(X, select = -c(X) )
  X = subset(X, select = -c(ferret) )
  
  
  
  
  #X=model.matrix(X)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
  
  ##pitch of targ is highly correlated with talker identity
  #modelreg_reduc5<- glm(realRelReleaseTimes ~  pitchoftarg*talker*stepval*side*timeToTarget*trialNum*pastcorrectresp*pastcatchtrial*AM*DaysSinceStart, data=df_animal, family = 'gaussian')
  
  
  mod_cv <- cv.glmnet(data.matrix(X), y=df_animal$realRelReleaseTimes, family = 'gaussian', intercept = F, alpha=1)
  lambda <- mod_cv$lambda.1se
  
  modelreg_reduc1<- glmnet(X, df_animal$realRelReleaseTimes,family = 'gaussian', lambda=lambda, intercept = F, alpha = 1)
  
  
  storemodall =coef(modelreg_reduc1)
  storecvall = mod_cv
  r2coeffall =(modelreg_reduc1$dev.ratio) #dev ratio represents the pseudo r^2
  #r2 <- fit$glmnet.fit$dev.ratio[which(modelreg_reduc1$glmnet.fit$lambda == modelreg_reduc1$lambda.1se)] 
  plot(mod_cv) 
  coef(mod_cv, c(mod_cv$lambda.min,
                 mod_cv$lambda.1se))
  
  coef(mod_cv, c(mod_cv$lambda.min))
  print(paste(mod_cv$lambda.min,
              log(mod_cv$lambda.min)))
  print(paste(mod_cv$lambda.1se,
              log(mod_cv$lambda.1se)))
  
  #preddata=predict(modelreg_reduc1, data.matrix(X), type='response')
  datamat = data.matrix(X)
  predictions_train <- predict(modelreg_reduc1, newx = data.matrix(X))
  eval_results(df_animal$realRelReleaseTimes, predictions_train, df_animal)
  
  
  plot(df_animal$realRelReleaseTimes, predictions_train, main="actual vs. predicted correct relative release times",
       xlab="actual ", ylab="predicted ", pch=19)
  abline(a=0, b=1)
  

  df_animal <- dfcatuse
  X = subset(df_animal, select = -c(correctresp) )
  X = subset(X, select = -c(X) )
  X = subset(X, select = -c(ferret) )
  
  
  #X=model.matrix(X)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
  
  ##pitch of targ is highly correlated with talker identity
  #modelreg_reduc5<- glm(realRelReleaseTimes ~  pitchoftarg*talker*stepval*side*timeToTarget*trialNum*pastcorrectresp*pastcatchtrial*AM*DaysSinceStart, data=df_animal, family = 'gaussian')
  
  
  mod_cv <- cv.glmnet(data.matrix(X), y=as.factor(df_animal$correctresp), family = 'binomial', intercept = F, alpha=1)
  lambda <- mod_cv$lambda.1se
  
  modelreg_reduc1<- glmnet(X, as.factor(df_animal$correctresp),family = 'binomial', lambda=lambda, intercept = F, alpha = 1)
  
  
  storemod_correctrespall =  coef(modelreg_reduc1)
  storecv_correctrespall = mod_cv
  i<-which(mod_cv$lambda == mod_cv$lambda.1se)
  e<-mod_cv$cvm[i]
  r2<-1-e/var((df_animal$correctresp))
  r2coeff_correctrespall =modelreg_reduc1$dev.ratio #dev ratio represents the pseudo r^2
  plot(mod_cv) 
  coef(mod_cv, c(mod_cv$lambda.min,
                 mod_cv$lambda.1se))
  
  coef(mod_cv, c(mod_cv$lambda.min))
  print(paste(mod_cv$lambda.min,
              log(mod_cv$lambda.min)))
  print(paste(mod_cv$lambda.1se,
              log(mod_cv$lambda.1se)))
  
  preddata=predict(modelreg_reduc1, data.matrix(X))
  
  
  plot(as.numeric(unlist(df_animal['correctresp'])), preddata, main="actual vs. predicted correct response",
       xlab="actual ", ylab="predicted ", pch=19)
  
  

