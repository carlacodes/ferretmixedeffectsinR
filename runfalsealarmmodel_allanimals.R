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
library(modelsummary)





df <- read.csv("falsealarmmodel_dfuse.csv")

#run an anova to figure out the best variables to used for the random effects
df$side=factor(df$side)
df$pitchoftarg=factor(df$pitchoftarg)
df$stepval=factor(df$stepval)
df$AM=factor(df$AM)
df$ferret=factor(df$ferret)
df$pastcatchtrial=factor(df$pastcatchtrial)
df$pastcorrectresp = factor(df$pastcorrectresp)
df$intra_trial_roving = factor(df$intra_trial_roving)


##fit individual model to each animal
#look at reaction time mixed effects model in humans or any other types of studies 
nullmodel1 <- lmer( falsealarm ~ 1 + (1|ferret), data = df, REML=FALSE)
nullmodel2 <- lmer( falsealarm ~ 1 + (1 + pastcorrectresp |ferret), data = df, REML=FALSE)
nullmodel22 <- lmer( falsealarm ~ 1 + (1 + pastcorrectresp |ferret)+(1 + trialNum |ferret), data = df, REML=FALSE)

nullmodel3 <- lmer( falsealarm ~ 1 +(1 +pastcorrectresp+pastcatchtrial |ferret), data = df, REML=FALSE)

nullmodel4 <- lmer( falsealarm ~ 1 +(0 +pastcorrectresp |ferret), data = df, REML=FALSE)

nullmodel5 <- lmer( falsealarm ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret), data = df, REML=FALSE)
nullmodel6 <- lmer( falsealarm ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret), data = df, REML=FALSE)

nullmodel7 <- lmer( falsealarm ~ 1 + (talker+targTimes+side |ferret), data = df, REML=FALSE)
nullmodel8 <- lmer( falsealarm ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret), data = df, REML=FALSE)
nullmodel9<- lmer( falsealarm ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret)+(0+intra_trial_roving|ferret), data = df, REML=FALSE)
nullmodel10 <- lmer( falsealarm ~ 1 + (talker+targTimes+side+intra_trial_roving |ferret), data = df, REML=FALSE)


nullmodel72 <- lmer( falsealarm ~ 1 + (talker |ferret), data = df, REML=FALSE)
nullmodel73 <- lmer( falsealarm ~ 1 + (talker+intra_trial_roving |ferret), data = df, REML=FALSE)
nullmodel74 <- lmer( falsealarm ~ 1 + (intra_trial_roving |ferret), data = df, REML=FALSE)
nullmodel75 <- lmer( falsealarm ~ 1 + (intra_trial_roving+side |ferret), data = df, REML=FALSE)
nullmodel76 <- lmer( falsealarm ~ 1 + (side |ferret), data = df, REML=FALSE)
nullmodel77 <- lmer( falsealarm ~ 1 + (talker+side |ferret), data = df, REML=FALSE)
nullmodel78 <- lmer( falsealarm ~ 1 + (talker+targTimes |ferret), data = df, REML=FALSE)
nullmodel79 <- lmer( falsealarm ~ 1 + (targTimes |ferret), data = df, REML=FALSE)


anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6, nullmodel7,nullmodel72, nullmodel73, nullmodel74, nullmodel75, nullmodel76,nullmodel77, nullmodel78, nullmodel79, nullmodel8, nullmodel22, nullmodel9, nullmodel10)



#now adding fixed effects 
modelreg_reduc1 <- lmer(
  falsealarm ~ pitchoftarg+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE, )#control = lmerControl(optimizer ="Nelder_Mead")

modelreg_reduc2 <- lmer(
  falsealarm ~ pitchoftarg+stepval+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE,)

modelreg_reduc3 <- lmer(
  falsealarm ~ pitchoftarg+stepval+talker+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE,)

modelreg_reduc4 <- lmer(
  falsealarm ~ pitchoftarg+stepval+talker+side+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE, )

modelreg_reduc5 <- lmer(
  falsealarm ~ pitchoftarg+stepval+talker+side+targTimes+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)

modelreg_reduc55 <- lmer(
  falsealarm ~ pitchoftarg*stepval+talker*stepval+side+targTimes+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = TRUE)

modelreg_reduc66 <- lmer(
  falsealarm ~ pitchoftarg+stepval+talker+side+targTimes+AM+trialNum+pastcorrectresp+pastcatchtrial+intra_trial_roving+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)
modelreg_reduc6 <- lmer(
  falsealarm ~ pitchoftarg+stepval+talker+side+targTimes+AM+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)
# talker*(pitchoftarg)+side + talker*stepval+targTimes
modelreg_reduc7 <- lmer(
  falsealarm ~ pitchoftarg*stepval+talker*pitchoftarg+side+targTimes+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)
modelreg_reduc72 <- lmer(
  falsealarm ~ pitchoftarg*stepval+talker*pitchoftarg+pitchoftarg*intra_trial_roving+side+targTimes+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)

modelreg_reduc8 <- lmer(
  falsealarm ~ pitchoftarg*stepval+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)
modelreg_reduc9 <- lmer(
  falsealarm ~ pitchoftarg*stepval*talker+trialNum+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)
modelreg_reduc10 <- lmer(
  falsealarm ~ pitchoftarg+stepval+trialNum+pastcorrectresp+pastcatchtrial+intra_trial_roving+(1 + pastcorrectresp |ferret),
  data=df, REML = FALSE)

anova(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3, modelreg_reduc4, modelreg_reduc5, modelreg_reduc55,modelreg_reduc6,modelreg_reduc66, modelreg_reduc7,modelreg_reduc72, modelreg_reduc8, modelreg_reduc9, modelreg_reduc10)
coeff=r2(modelreg_reduc72)
#declare chosen model HERE:
chosen_model <- modelreg_reduc72

set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          title.size=1.5,
          axis.title.size = 0.5,  #To change axis title size
          axis.textsize.x = 1,  #To change x axis text size
          axis.textsize.y = 1)  #To change y axis text size

forestplot <- plot_model(chosen_model,show.values = TRUE, type = 're', value.offset = 0.5, title = 'Ranked features of the false alarm model')
forestplot2 <- modelplot(chosen_model)

forestplot2 <- modelplot(chosen_model) +theme(axis.title.x = element_text(size = 12, vjust = -0.5))+    aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("blue", "red"))



library(ggplot2)
library(gridExtra)
library(modelplotr)
library(lmerTest)
library(grid)



# Add the table to the forest plot using annotation_custom
coef_table <- fixef(chosen_model)
coef_table <-rev(coef_table)
coef_table[] <- lapply(coef_table, function(x) if(is.numeric(x)) round(x, 3) else x)

coef_table <- data.frame(coef_table, confint(chosen_model)[,1], confint(chosen_model)[,2])
names(coef_table) <- c("Estimate", "CI_low", "CI_high")

# Add SE column to the table
coef_table$SE <- attr(summary(chosen_model)$coefficients, "std.err")
#coef_table$Estimate <- round(coef_table$Estimate, 2)
#
# Create a transparent, rounded table grob
coef_table <- coef_table[nrow(coef_table):1, ]

table_grob <- tableGrob(
  coef_table,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 7,
    padding = unit(c(2, 4.5), "mm"),
    fg_params = list(hjust = 0, x = 0.1),
    bg_params = list(fill = alpha("white", 0))
  )
)


totalplot <- forestplot2  +labs(x = 'Coefficients',  y = 'Term names',  title = 'Coefficients for predicting a \n false alarm or hit response', color  = '') 


ggsave(filename = "D:/behavmodelfigs/mixedeffectsmodels/falsealarm_modelforestplot.png", plot = totalplot, width = 7, height = 10)
  
# Save the plot as a JPEG file
ggsave(filename = "D:/behavmodelfigs/mixedeffectsmodels/falsealarm_modelforestplot2.png", plot = forestplot2, width = 7, height = 10)
dev.off()


summary(chosen_model)
oneferret=subset(df, ferret == 1)
zoladata=subset(df, ferret==0)
tinadata=subset(df, ferret==2)
macdata=subset(df, ferret==3)
cruellact=oneferret['falsealarm']

cruellapred=predict(chosen_model, oneferret, type='response')
zolapred=predict(chosen_model, zoladata, type='response')
tinapred=predict(chosen_model, tinadata, type='response')
macpred=predict(chosen_model, macdata, type='response')


plot(as.numeric(unlist(oneferret['falsealarm'])), cruellapred, main="Cruella actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(zoladata['falsealarm'])), zolapred, main="Zola actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(tinadata['falsealarm'])), tinapred, main="Tina actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)

plot(as.numeric(unlist(macdata['falsealarm'])), macpred, main="Mac actual vs. predicted correct responses",
     xlab="actual ", ylab="predicted ", pch=19)
abline(a=0, b=1)
