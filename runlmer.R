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
library(dplyr)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(coefplot)
library(Hmisc)
library(modelsummary)




dfuse <- read.csv("dfuse.csv") 
dfcatuse <- read.csv("dfcat_use.csv")
#dfcattest <- read.csv('mergedtrainedandnaive.csv')

df <- read.csv("df.csv")
dfcat <- read.csv("dfcat.csv")


var.labels = c(pitchoftarg="Pitch of Target", stepval="Change in pitch from precursor to target", pastcatchtrial = "Trial n-1 was catch")

label(df) = as.list(var.labels[match(names(df), names(var.labels))])
label(df$pastcatchtrial)

#run an anova to figure out the best variables to used for the random effects
df$side=factor(df$side)
df$pitchoftarg=factor(df$pitchoftarg)
df$stepval=factor(df$stepval)
df$AM=factor(df$AM)
df$ferret=factor(df$ferret)
df$pastcatchtrial=factor(df$pastcatchtrial)
df$pastcorrectresp = factor(df$pastcorrectresp)
df$precur_and_targ_same = factor(df$precur_and_targ_same)

df$realRelReleaseTimes = log(df$realRelReleaseTimes)



##fit individual model to each animal
#look at reaction time mixed effects model in humans or any other types of studies 
nullmodel1 <- lmer( realRelReleaseTimes ~ 1 + (1|ferret), data = df, REML=FALSE)
nullmodel2 <- lmer( realRelReleaseTimes ~ 1 + (1 + pastcorrectresp |ferret), data = df, REML=FALSE)
nullmodel22 <- lmer( realRelReleaseTimes ~ 1 + (1 + pastcorrectresp |ferret)+(1 + trialNum |ferret), data = df, REML=FALSE)

nullmodel3 <- lmer( realRelReleaseTimes ~ 1 +(1 +pastcorrectresp+pastcatchtrial |ferret), data = df, REML=FALSE)

nullmodel4 <- lmer( realRelReleaseTimes ~ 1 +(0 +pastcorrectresp |ferret), data = df, REML=FALSE)

nullmodel5 <- lmer( realRelReleaseTimes ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret), data = df, REML=FALSE)
nullmodel6 <- lmer( realRelReleaseTimes ~ 1 + (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret), data = df, REML=FALSE)

nullmodel7 <- lmer( realRelReleaseTimes ~ 1 + (talker+timeToTarget+side |ferret), data = df, REML=FALSE)
nullmodel72 <- lmer( realRelReleaseTimes ~ 1 + (talker |ferret), data = df, REML=FALSE)
nullmodel73 <- lmer( realRelReleaseTimes ~ 1 + (talker+precur_and_targ_same |ferret), data = df, REML=FALSE)
nullmodel74 <- lmer( realRelReleaseTimes ~ 1 + (precur_and_targ_same |ferret), data = df, REML=FALSE)
nullmodel75 <- lmer( realRelReleaseTimes ~ 1 + (precur_and_targ_same+side |ferret), data = df, REML=FALSE)
nullmodel76 <- lmer( realRelReleaseTimes ~ 1 + (side |ferret), data = df, REML=FALSE)
nullmodel77 <- lmer( realRelReleaseTimes ~ 1 + (talker+side |ferret), data = df, REML=FALSE)
nullmodel78 <- lmer( realRelReleaseTimes ~ 1 + (talker+timeToTarget |ferret), data = df, REML=FALSE)
nullmodel79 <- lmer( realRelReleaseTimes ~ 1 + (timeToTarget |ferret), data = df, REML=FALSE)




nullmodel8 <- lmer( realRelReleaseTimes ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret), data = df, REML=FALSE)
nullmodel9<- lmer( realRelReleaseTimes ~ 1 + (0 +side |ferret)+(0 +talker |ferret)+(0 +AM |ferret)+(0+precur_and_targ_same|ferret), data = df, REML=FALSE)
nullmodel10 <- lmer( realRelReleaseTimes ~ 1 + (talker+timeToTarget+side+precur_and_targ_same |ferret), data = df, REML=FALSE)

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5, nullmodel6, nullmodel7,nullmodel72, nullmodel73, nullmodel74, nullmodel75, nullmodel76,nullmodel77, nullmodel78, nullmodel79, nullmodel8, nullmodel22, nullmodel9, nullmodel10)




#now adding fixed effects 
modelreg_reduc1 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE, )#control = lmerControl(optimizer ="Nelder_Mead")

modelreg_reduc2 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE,)

modelreg_reduc3 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE,)

modelreg_reduc4 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE, )

modelreg_reduc5 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE)

modelreg_reduc55 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+talker*stepval+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = TRUE)

modelreg_reduc6 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+AM+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE)

modelreg_reduc66 <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval+talker+side+timeToTarget+AM+trialNum+pastcorrectresp+pastcatchtrial+precur_and_targ_same+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE)

# talker*(pitchoftarg)+side + talker*stepval+timeToTarget
modelreg_reduc7 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+talker*pitchoftarg+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE)
modelreg_reduc72 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+talker*pitchoftarg+pitchoftarg*precur_and_targ_same+side+timeToTarget+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE)
modelreg_reduc8 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval+trialNum+pastcorrectresp+pastcatchtrial+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE)
modelreg_reduc9 <- lmer(
  realRelReleaseTimes ~ pitchoftarg*stepval*talker+trialNum+ (0 +pastcorrectresp |ferret)+(0 +talker |ferret)+(0 +trialNum |ferret),
  data=df, REML = FALSE)


anova(modelreg_reduc1, modelreg_reduc2, modelreg_reduc3, modelreg_reduc4, modelreg_reduc5, modelreg_reduc55,modelreg_reduc6,modelreg_reduc66, modelreg_reduc7, modelreg_reduc72, modelreg_reduc8, modelreg_reduc9)
coeff=r2(modelreg_reduc66)
#declare chosen model HERE:
chosen_model <- modelreg_reduc66


oneferret=subset(df, ferret == 1)
zoladata=subset(df, ferret==0)
tinadata=subset(df, ferret==2)
macdata=subset(df, ferret==3)
cruellact=oneferret['realRelReleaseTimes']

cruellapred=predict(chosen_model, oneferret, type='response')
zolapred=predict(chosen_model, zoladata, type='response')
tinapred=predict(chosen_model, tinadata, type='response')
macpred=predict(chosen_model, macdata, type='response')


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



set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          title.size=1,
          axis.title.size = 0.5,  #To change axis title size
          axis.textsize.x = 1,  #To change x axis text size
          axis.textsize.y = 1)  #To change y axis text size

list_of_coeffs <- coef(summary(chosen_model))

estimates <- as.vector(list_of_coeffs[1:15,1])


forestplot <- plot_model(chosen_model,show.values = TRUE, value.offset = 0.5, title = 'Ranked features of the release times model for the subset of correct responses') + 
  scale_x_discrete(labels = c("pitch of target", "step value", "side", "AM", "past response was correct", "past trial was catch", 
                                "precursor = target pitch", "ferret ID", "pitch of target = 109 Hz", "pitch of targ = 124 Hz", "pitch of target = 144 Hz", "pitch of target = 191 Hz", "pitch of target = 251 Hz", "low to high step in pitch"))

forestplot <-plot_model(chosen_model)
forestplot2 <- modelplot(chosen_model)


library(ggplot2)
library(gridExtra)
library(modelplotr)
library(lmerTest)
library(grid)



# Add the table to the forest plot using annotation_custom
coef_table <- fixef(chosen_model)
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
    padding = unit(c(2, 5), "mm"),
    fg_params = list(hjust = 0, x = 0.1),
    bg_params = list(fill = alpha("white", 0))
  )
)

table_grob <- rev(table_grob)

forestplot2 + annotation_custom(table_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)


# Extract coefficient table for fixed effects
coef_table <- fixef(chosen_model)
coef_table <- data.frame(coef_table, confint(chosen_model)[,1], confint(chosen_model)[,2])
names(coef_table) <- c("Estimate", "CI_low", "CI_high")



# Add SE column to the table
coef_table$SE <- attr(summary(chosen_model)$coefficients, "std.err")

# Add the table to the forest plot using annotation_custom
table_grob <- tableGrob(coef_table, rows = NULL)

table_format <- ttheme_default(base_size = 8, padding = unit(c(2, 2), "mm"))

# Round table values to 2 decimal points
coef_table[, 1:3] <- round(coef_table[, 1:3], 2)

# Create table grob with formatted cells and transparent background
table_grob <- tableGrob(
  coef_table, rows = NULL,
  theme = table_format, 
  gp = gpar(fill = "transparent")
)


forestplot2 + annotation_custom(table_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

forestplot3 <- coefplot(chosen_model, main = "Coefficients2", overlay = TRUE, overlay.text = (coef(chosen_model)))


# Save the plot as a JPEG file

ggsave(filename = "D:/behavmodelfigs/mixedeffectsmodels/correctreleasetimes_modelforestplot3.png", plot = forestplot, width = 7, height = 10)
ggsave(filename = "D:/behavmodelfigs/mixedeffectsmodels/correctreleasetimes_modelforestplot_original22.png", plot = forestplot2, width = 7, height = 10)

dev.off()


# set_theme(base = theme_classic(), #To remove the background color and the grids
#           theme.font = 'serif',   #To change the font type
#           title.size=1,
#           axis.title.size = 0.5,  #To change axis title size
#           axis.textsize.x = 1,  #To change x axis text size
#           axis.textsize.y = 1)  #To change y axis text size
# 
# plot_data <- coef(chosen_model) %>%
#   as.data.frame() %>%
#   mutate(term = row.names(.))
# 
# ggplot(plot_data, aes(x = estimate, y = term)) +
#   geom_point() +
#   geom_errorbarh(aes(xminq