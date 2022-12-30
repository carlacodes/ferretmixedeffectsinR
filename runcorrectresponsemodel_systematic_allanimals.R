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
library(cAIC4) # for the stepAIC() functionWARNINGS()





df <- read.csv("correctresponsemodel_dfuse.csv")
df <- df[c("ferret", "correctresp", "pitchoftarg","talker","stepval", "AM", "side", "timeToTarget","precur_and_targ_same","trialNum", "pastcorrectresp", "pastcatchtrial")]


#run an anova to figure out the best variables to used for the random effects
df$side=factor(df$side)
df$pitchoftarg=factor(df$pitchoftarg)
df$stepval=factor(df$stepval)
df$AM=factor(df$AM)
df$ferret=factor(df$ferret)
df$pastcatchtrial=factor(df$pastcatchtrial)
df$pastcorrectresp = factor(df$pastcorrectresp)
df$precur_and_targ_same = factor(df$precur_and_targ_same)
df <- as.data.frame(df)


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
nullmodel9<- lmer( correctresp ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret)+(0+precur_and_targ_same|ferret), data = df, REML=FALSE)
nullmodel10 <- lmer( correctresp ~ 1 + (talker+timeToTarget+side+precur_and_targ_same |ferret), data = df, REML=FALSE)

nullmodel72 <- lmer( correctresp ~ 1 + (talker |ferret), data = df, REML=FALSE)
nullmodel73 <- lmer( correctresp ~ 1 + (talker+precur_and_targ_same |ferret), data = df, REML=FALSE)
nullmodel74 <- lmer( correctresp ~ 1 + (precur_and_targ_same |ferret), data = df, REML=FALSE)
nullmodel75 <- lmer( correctresp ~ 1 + (precur_and_targ_same+side |ferret), data = df, REML=FALSE)
nullmodel76 <- lmer( correctresp ~ 1 + (side |ferret), data = df, REML=FALSE)
nullmodel77 <- lmer( correctresp ~ 1 + (talker+side |ferret), data = df, REML=FALSE)
nullmodel78 <- lmer( correctresp ~ 1 + (talker+timeToTarget |ferret), data = df, REML=FALSE)
nullmodel79 <- lmer( correctresp ~ 1 + (timeToTarget |ferret), data = df, REML=FALSE)
anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6, nullmodel7,nullmodel72, nullmodel73, nullmodel74, nullmodel75, nullmodel76,nullmodel77, nullmodel78, nullmodel79, nullmodel8, nullmodel22, nullmodel9, nullmodel10)

model <- lmer(correctresp ~ . + (1 |ferret), data = df)
#talker+timeToTarget+side+precur_and_targ_same


model_selected<-stepcAIC(model)

#declare chosen model HERE:
chosen_model <- model_selected$finalModel
coeff=r2(chosen_model)


summary(model_selected)

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

plot_model(chosen_model, show.values = TRUE, value.offset = 0.3,title = 'Ranked features of the correct response model of the subset of the correct responses')
png(file="D:/behavmodelfigs/mixedeffectsmodels/correctresponsemodelforestplot.png",
    width=600, height=350)
dev.off()
