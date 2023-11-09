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
df$precur_and_targ_same = factor(df$precur_and_targ_same)

unique_values_response <- unique(df$response)
print(unique_values_response)
##fit individual model to each animal
#look at reaction time mixed effects model in humans or any other types of studies 
nullmodel1 <- glmer( correctresp ~ 1 + (1|ferret), data = df, family=binomial)
nullmodel2 <- glmer( correctresp ~ 1 + (1 + pastcorrectresp |ferret), data = df, family=binomial)
nullmodel22 <- glmer( correctresp ~ 1 + (1 + pastcorrectresp |ferret)+(1 + trialNum |ferret), data = df, family=binomial)

nullmodel3 <- glmer( correctresp ~ 1 +(1 +pastcorrectresp+pastcatchtrial |ferret), data = df, family=binomial)

nullmodel4 <- glmer( correctresp ~ 1 +(0 +pastcorrectresp |ferret), data = df, family=binomial)

nullmodel5 <- glmer( correctresp ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret), data = df, family=binomial)
nullmodel6 <- glmer( correctresp ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret), data = df, family=binomial)

nullmodel7 <- glmer( correctresp ~ 1 + (talker+timeToTarget+side |ferret), data = df, family=binomial)
nullmodel8 <- glmer( correctresp ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret), data = df, family=binomial)
nullmodel9<- glmer( correctresp ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret)+(0+precur_and_targ_same|ferret), data = df, family=binomial)
nullmodel10 <- glmer( correctresp ~ 1 + (talker+timeToTarget+side+precur_and_targ_same |ferret), data = df, family=binomial)

nullmodel72 <- glmer( correctresp ~ 1 + (talker |ferret), data = df, family=binomial)
nullmodel73 <- glmer( correctresp ~ 1 + (talker+precur_and_targ_same |ferret), data = df, family=binomial)
nullmodel74 <- glmer( correctresp ~ 1 + (precur_and_targ_same |ferret), data = df, family=binomial)
nullmodel75 <- glmer( correctresp ~ 1 + (precur_and_targ_same+side |ferret), data = df, family=binomial)
nullmodel76 <- glmer( correctresp ~ 1 + (side |ferret), data = df, family=binomial)
nullmodel77 <- glmer( correctresp ~ 1 + (talker+side |ferret), data = df, family=binomial)
nullmodel78 <- glmer( correctresp ~ 1 + (talker+targTimes |ferret), data = df, family=binomial)
nullmodel79 <- glmer( correctresp ~ 1 + (targTimes |ferret), data = df, family=binomial)
anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6, nullmodel7,nullmodel72, nullmodel73, nullmodel74, nullmodel75, nullmodel76,nullmodel77, nullmodel78, nullmodel79, nullmodel8, nullmodel22, nullmodel9, nullmodel10)




#now adding fixed effects 
modelreg_reduc1 <- glmer(
  correctresp ~ pitchoftarg+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial, )#control = lmerControl(optimizer ="Nelder_Mead")

modelreg_reduc2 <- glmer(
  correctresp ~ pitchoftarg+stepval+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial,)

modelreg_reduc3 <- glmer(
  correctresp ~ pitchoftarg+stepval+talker+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial,)

modelreg_reduc4 <- glmer(
  correctresp ~ pitchoftarg+stepval+talker+side+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial, )

modelreg_reduc5 <- glmer(
  correctresp ~ pitchoftarg+stepval+talker+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)

modelreg_reduc55 <- glmer(
  correctresp ~ pitchoftarg*stepval+talker*stepval+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)

modelreg_reduc66 <- glmer(
  correctresp ~ pitchoftarg+stepval+talker+side+timeToTarget+AM+trialNum+pastcorrectresp+pastcatchtrial+precur_and_targ_same+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)
modelreg_reduc6 <- glmer(
  correctresp ~ pitchoftarg+stepval+talker+side+timeToTarget+AM+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)
# talker*(pitchoftarg)+side + talker*stepval+timeToTarget
modelreg_reduc7 <- glmer(
  correctresp ~ pitchoftarg*stepval+talker*pitchoftarg+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)
modelreg_reduc72 <- glmer(
  correctresp ~ pitchoftarg*stepval+talker*pitchoftarg+pitchoftarg*precur_and_targ_same+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)

modelreg_reduc8 <- glmer(
  correctresp ~ pitchoftarg*stepval+trialNum+pastcorrectresp+pastcatchtrial+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)
modelreg_reduc9 <- glmer(
  correctresp ~ pitchoftarg*stepval*talker+trialNum+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)
modelreg_reduc10 <- glmer(
  correctresp ~ pitchoftarg+stepval+trialNum+pastcorrectresp+pastcatchtrial+precur_and_targ_same+(1 + pastcorrectresp |ferret),
  data=df, family=binomial)

anova(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3, modelreg_reduc4, modelreg_reduc5, modelreg_reduc55,modelreg_reduc6,modelreg_reduc66, modelreg_reduc7,modelreg_reduc72, modelreg_reduc8, modelreg_reduc9, modelreg_reduc10)
coeff=r2(modelreg_reduc4)
#declare chosen model HERE:
chosen_model <- modelreg_reduc4


s <- simulate(chosen_model)
# proportion of response values that equal simulated responses
mean(df$correctresp == s)
threshold <- 0.5
s_binary <- ifelse(s >= threshold, 1, 0)

# Create a confusion matrix
conf_matrix <- confusionMatrix(data = factor(s_binary), reference = factor(df$falsealarm))

# Print the confusion matrix
print(conf_matrix)
png('D:\behavmodelfigs\mixedeffectsmodels/confusionmatrix_correctresponsemodel.png')
fourfoldplot(as.table(conf_matrix),color=c("purple","red"),main = "Confusion Matrix for the Correct Response/Miss Model")
dev.off()

oneferret=subset(df, ferret == 1)
zoladata=subset(df, ferret==0)
tinadata=subset(df, ferret==2)
macdata=subset(df, ferret==3)
cruellact=oneferret['correctresp']

cruellapred=predict(chosen_model, oneferret, type='response')
zolapred=predict(chosen_model, zoladata, type='response')
tinapred=predict(chosen_model, tinadata, type='response')
macpred=predict(chosen_model, macdata, type='response')

png(filename="D:/behavmodelfigs/mixedeffectsmodels/originalvspredicted_CR_or_not_ontargettrials_F1815.png")
plot(as.numeric(unlist(oneferret['correctresp'])), cruellapred, main="Cruella actual vs. predicted responses for the CR/miss model",
     xlab="actual ", ylab="predicted ", pch=19)
dev.off()

png(filename="D:/behavmodelfigs/mixedeffectsmodels/originalvspredicted_CR_or_not_ontargettrial_F1702.png")
plot(as.numeric(unlist(zoladata['correctresp'])), zolapred, main="Zola actual vs. predicted responses for the CR/miss model",
     xlab="actual ", ylab="predicted ", pch=19)#
dev.off()

png(filename="D:/behavmodelfigs/mixedeffectsmodels/originalvspredicted_CR_or_not_ontargettrial_F1803.png")
plot(as.numeric(unlist(tinadata['correctresp'])), tinapred, main="Tina actual vs. predicted responses for the CR/miss model",
     xlab="actual ", ylab="predicted ", pch=19)
dev.off()

png(filename="D:/behavmodelfigs/mixedeffectsmodels/originalvspredicted_CR_or_not_ontargettrial_F2002.png")
plot(as.numeric(unlist(macdata['correctresp'])), macpred, main="Mac actual vs. predicted responses for the CR/miss model",
     xlab="actual ", ylab="predicted ", pch=19)
dev.off()

set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          title.size=1.5,
          axis.title.size = 0.5,  #To change axis title size
          axis.textsize.x = 1,  #To change x axis text size
          axis.textsize.y = 1)  #To change y axis text size

forestplot <- plot_model(chosen_model,show.values = TRUE, value.offset = 0.5, title = 'Ranked features of the correct response model')

# Save the plot as a JPEG file
ggsave(filename = "D:/behavmodelfigs/mixedeffectsmodels/correctresponse_modelforestplot22.png", plot = forestplot, width = 7, height = 10)

# forestplot2 <- modelplot(chosen_model) +theme(axis.title.x = element_text(size = 12, vjust = -0.5))+   aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) + scale_color_manual(values = c("blue", "red"))


forestplot2 <- modelplot(chosen_model) +theme(axis.title.x = element_text(size = 12, vjust = -0.5))+   scale_y_discrete(
  labels = rev(c("past trial was catch", "past response was correct", "trial Number", "right side", "pos. change in pitch", "no change in pitch", "pitch = 251 Hz", "pitch = 191 Hz", "pitch = 144 Hz", "pitch = 124 Hz", "pitch = 109 Hz")))+   aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) + scale_color_manual(values = c("blue", "red"))



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


totalplot <- forestplot2  +labs(x = 'Coefficient value',  y = 'Coefficient name',  title = 'Coefficients for model predicting a hit or miss', color  = '') 


ggsave(filename = "D:/behavmodelfigs/mixedeffectsmodels/correctresponses_14082023.png", plot = totalplot, width = 7, height = 10)
dev.off()

