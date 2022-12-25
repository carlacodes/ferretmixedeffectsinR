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




df <- read.csv("correctresponsemodel_dfuse.csv")

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
nullmodel1 <- lmer( correctresp ~ 1 + (1|ferret), data = df, REML=FALSE)
nullmodel2 <- lmer( correctresp ~ 1 + (1 + pastcorrectresp |ferret), data = df, REML=FALSE)
nullmodel22 <- lmer( correctresp ~ 1 + (1 + pastcorrectresp |ferret)+(1 + trialNum |ferret), data = df, REML=FALSE)

nullmodel3 <- lmer( correctresp ~ 1 +(1 +pastcorrectresp+pastcatchtrial |ferret), data = df, REML=FALSE)

nullmodel4 <- lmer( correctresp ~ 1 +(0 +pastcorrectresp |ferret), data = df, REML=FALSE)

nullmodel5 <- lmer( correctresp ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret), data = df, REML=FALSE)
nullmodel6 <- lmer( correctresp ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret), data = df, REML=FALSE)

nullmodel7 <- lmer( correctresp ~ 1 + (talker+timeToTarget+side |ferret), data = df, REML=FALSE)
nullmodel8 <- lmer( correctresp ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret), data = df, REML=FALSE)

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6, nullmodel7, nullmodel8, nullmodel22)




#now adding fixed effects 
modelreg_reduc1 <- lmer(
  correctresp ~ pitchoftarg+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE, )#control = lmerControl(optimizer ="Nelder_Mead")

modelreg_reduc2 <- lmer(
  correctresp ~ pitchoftarg+stepval+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE,)

modelreg_reduc3 <- lmer(
  correctresp ~ pitchoftarg+stepval+talker+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE,)

modelreg_reduc4 <- lmer(
  correctresp ~ pitchoftarg+stepval+talker+side+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE, )

modelreg_reduc5 <- lmer(
  correctresp ~ pitchoftarg+stepval+talker+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE)

modelreg_reduc55 <- lmer(
  correctresp ~ pitchoftarg*stepval+talker*stepval+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = TRUE)

modelreg_reduc6 <- lmer(
  correctresp ~ pitchoftarg+stepval+talker+side+timeToTarget+AM+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE)
# talker*(pitchoftarg)+side + talker*stepval+timeToTarget
modelreg_reduc7 <- lmer(
  correctresp ~ pitchoftarg*stepval+talker*pitchoftarg+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE)

modelreg_reduc8 <- lmer(
  correctresp ~ pitchoftarg*stepval+trialNum+pastcorrectresp+pastcatchtrial+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE)
modelreg_reduc9 <- lmer(
  correctresp ~ pitchoftarg*stepval*talker+trialNum+(talker+timeToTarget+side |ferret),
  data=df, REML = FALSE)


anova(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3, modelreg_reduc4, modelreg_reduc5, modelreg_reduc55,modelreg_reduc6, modelreg_reduc7, modelreg_reduc8, modelreg_reduc9)
coeff=r2(modelreg_reduc6)
#declare chosen model HERE:
chosen_model <- modelreg_reduc6


oneferret=subset(df, ferret == 1)
zoladata=subset(df, ferret==0)
tinadata=subset(df, ferret==2)
macdata=subset(df, ferret==3)
cruellact=oneferret['correctresp']

cruellapred=predict(chosen_model, oneferret, type='response')
zolapred=predict(chosen_model, zoladata, type='response')
tinapred=predict(chosen_model, tinadata, type='response')
macpred=predict(chosen_model, macdata, type='response')


plot(as.numeric(unlist(oneferret['correctresp'])), cruellapred, main="Cruella actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(zoladata['correctresp'])), zolapred, main="Zola actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(tinadata['correctresp'])), tinapred, main="Tina actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(macdata['correctresp'])), macpred, main="Mac actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)
max(df$pastcatchtrial)
set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          title.size=2,
          axis.title.size = 1.5,  #To change axis title size
          axis.textsize.x = 1.2,  #To change x axis text size
          axis.textsize.y = 1.2)  #To change y axis text size

plot_model(chosen_model, title = 'Ranked features of the correct response model of the subset of the correct responses')
png(file="D:/behavmodelfigs/mixedeffectsmodels/correctresponsemodelforestplot.png",
    width=600, height=350)
dev.off()
