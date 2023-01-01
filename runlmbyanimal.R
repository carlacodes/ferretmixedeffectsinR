
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
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

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
df_fa <- read.csv("falsealarmmodel_dfuse.csv")
df_correctresp <- read.csv("correctresponsemodel_dfuse.csv")


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
df$precur_and_targ_same = factor(df$precur_and_targ_same)

df_correctresp$side=factor(df_correctresp$side)
df_correctresp$talker=factor(df_correctresp$talker)
df_correctresp$pitchoftarg=factor(df_correctresp$pitchoftarg)
df_correctresp$stepval=factor(df_correctresp$stepval)
df_correctresp$AM=factor(df_correctresp$AM)
df_correctresp$ferret=factor(df_correctresp$ferret)
df_correctresp$pastcatchtrial=factor(df_correctresp$pastcatchtrial)
df_correctresp$pastcorrectresp = factor(df_correctresp$pastcorrectresp)
df_correctresp$precur_and_targ_same = factor(df_correctresp$precur_and_targ_same)

df_fa$side=factor(df_fa$side)
df_fa$talker=factor(df_fa$talker)
df_fa$pitchoftarg=factor(df_fa$pitchoftarg)
df_fa$stepval=factor(df_fa$stepval)
df_fa$AM=factor(df_fa$AM)
df_fa$ferret=factor(df_fa$ferret)
df_fa$pastcatchtrial=factor(df_fa$pastcatchtrial)
df_fa$pastcorrectresp = factor(df_fa$pastcorrectresp)
df_fa$intra_trial_roving = factor(df_fa$intra_trial_roving)



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


dfcorrectresponse <- df_correctresp[c("ferret", "AM", "correctresp", "pitchoftarg","talker","stepval", "side", "timeToTarget","precur_and_targ_same","trialNum", "pastcorrectresp", "pastcatchtrial")]
df_falsealarm <-df_fa[c("ferret", "falsealarm", "AM", "pitchoftarg","talker","stepval", "side", "timeToTarget","intra_trial_roving","trialNum", "pastcorrectresp", "pastcatchtrial")]
dfsmall <- df[c("ferret", "realRelReleaseTimes","AM",  "pitchoftarg","talker","stepval", "side", "timeToTarget","precur_and_targ_same","trialNum", "pastcorrectresp", "pastcatchtrial")]

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

modelstore_releasetimes=list()
r2list_releasetimes=list()
ferret_list=c('F1702', 'F1815', 'F1803', 'F2002')

for (i in 0:3) {
  print(i) 
  df_animal <- subset(dfsmall, ferret == i)
  #X = subset(df_animal, select = -c(realRelReleaseTimes) )
  X = subset(df_animal, select = -c(ferret) )
  X<-as.data.frame(X)
  
  
  #X=model.matrix(X)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
  
  ##pitch of targ is highly correlated with talker identity
  #modelreg_reduc5<- glm(realRelReleaseTimes ~  pitchoftarg*talker*stepval*side*timeToTarget*trialNum*pastcorrectresp*pastcatchtrial*AM*DaysSinceStart, data=df_animal, family = 'gaussian')
  
  
  # Create the full model with all predictor variables
  full_model <- lm(realRelReleaseTimes ~ ., data = X)
  
  # Use the step() function to select the best model using ANOVA
  best_model_releasetimes <- step(full_model, direction = "backward", scope = formula(full_model), k = 2, trace = 0)
  
  # Print the summary of the best model
  summary(best_model_releasetimes)
  
  modelstore_releasetimes<- append(modelstore_releasetimes, best_model_releasetimes) 
  r2list_releasetimes <-append(r2list_releasetimes,  r2(best_model_releasetimes)) 
  
  
  preddata=predict(best_model_releasetimes, X)
  coeffrank <- plot_model(best_model_releasetimes, show.values = TRUE, value.offset = 0.3,title =  (paste("Release time model for", ferret_list[i+1])))
  ggsave(filename = paste(ferret_list[i+1],"rankedcoeefreleasetime.png"), plot = coeffrank, width =5, height=5)
  
  plot(as.numeric(unlist(df_animal['realRelReleaseTimes'])), preddata, main="actual vs. predicted correct response",
       xlab="actual ", ylab="predicted ", pch=19)
  
  
  
  
}

modelstore_correctresponse=list()
r2list_correctresponse=list()
for (i in 0:3) {
  print(i) 
  df_animal <- subset(dfcorrectresponse, ferret == i)
  #X = subset(df_animal, select = -c(correctresp) )
  X = subset(df_animal, select = -c(ferret) )
  X = subset(df_animal, select = -c(ferret) )
  
  X<-as.data.frame(X)

  
  
  #X=model.matrix(X)
  #df_animal <- df_animal[c( "realRelReleaseTimes", "pitchoftarg","talker","stepval", "side", "timeToTarget", "trialNum", "pastcorrectresp", "pastcatchtrial")]
  
  ##pitch of targ is highly correlated with talker identity
  #modelreg_reduc5<- glm(realRelReleaseTimes ~  pitchoftarg*talker*stepval*side*timeToTarget*trialNum*pastcorrectresp*pastcatchtrial*AM*DaysSinceStart, data=df_animal, family = 'gaussian')
  
 
  # Create the full model with all predictor variables
  full_model <- glm(correctresp ~ ., data = X, family='binomial')
  
  # Use the step() function to select the best model using ANOVA
  best_model <- step(full_model, direction = "backward", scope = formula(full_model), k = 2, trace = 0)
  
  # Print the summary of the best model
  summary(best_model)
  
  modelstore_correctresponse = append(modelstore_correctresponse, best_model)
  r2list_correctresponse = append(r2list_correctresponse, r2(best_model))
  

  
  preddata=predict(best_model, X)
  
  coeffrank<- plot_model(best_model, show.values = TRUE, value.offset = 0.3,title =  (paste("Correct response model for ", ferret_list[i+1])))
  ggsave(filename = paste(ferret_list[i+1],"correctresponse_rankedcoeef.png"), plot = coeffrank, width = 5, height= 7)
  
  
  plot(as.numeric(unlist(df_animal['correctresp'])), preddata, main="actual vs. predicted correct response",
       xlab="actual ", ylab="predicted ", pch=19)

  
  
  
}

modelstore_falsealarm=list()
r2list_falsealarm=list()
for (i in 0:3) {
  print(i) 
  df_animal <- subset(df_falsealarm, ferret == i)
  X = subset(df_animal, select = -c(ferret) )

  X<-as.data.frame(X)
  
  
  
  
  
  # Create the full model with all predictor variables
  full_model <- glm(falsealarm ~ ., data = X, family='binomial')
  
  # Use the step() function to select the best model using ANOVA
  best_model <- step(full_model, direction = "backward", scope = formula(full_model), k = 2, trace = 0)
  
  # Print the summary of the best model
  summary(best_model)
  
  modelstore_falsealarm = append(modelstore_falsealarm, best_model)
  r2list_falsealarm = append(r2list_falsealarm, r2(best_model))
  
  
  preddata=predict(best_model, X)
  
  coeffrank <- plot_model(best_model, show.values = TRUE, value.offset = 0.3,title =  (paste("False alarm model for ", ferret_list[i+1])))
  ggsave(filename = paste(ferret_list[i+1],"rankedcoeef.png"), plot = coeffrank, width =5, height=5)
  plot.new()
  predversusactual<-plot(as.numeric(unlist(df_animal['falsealarm'])), preddata, main="actual vs. predicted response",
       xlab="actual ", ylab="predicted ", pch=19)
  
  
  
  
}



