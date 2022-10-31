## load lme4, JWileymisc, and multilevelTools packages
## (i.e., "open the 'apps' ") 
library(lme4)
#> Loading required package: Matrix
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
nullmodel1 <- lmer( realRelReleaseTimes ~ 1 + (1|ferret), data = df, REML=FALSE)
nullmodel2 <- lmer( realRelReleaseTimes ~ 1 + (1 + timeToTarget |ferret), data = df, REML=FALSE)
nullmodel3 <- lmer( realRelReleaseTimes ~ 1 + (1 + side* timeToTarget |ferret), data = df, REML=FALSE)
nullmodel4 <- lmer( realRelReleaseTimes ~ 1 + (1 + side* timeToTarget*AM |ferret), data = df, REML=FALSE)
nullmodel5 <- lmer( realRelReleaseTimes ~ 1 + (1 + side+ timeToTarget+AM |ferret), data = df, REML=FALSE)

anova (nullmodel1, nullmodel2, nullmodel3, nullmodel4, nullmodel5)

modelreg_reduc <- lmer(
  realRelReleaseTimes ~ pitchoftarg+stepval + talker + side+ (1 + side* timeToTarget |ferret),
  data=df, REML = FALSE)

r2coeff <-r2_nakagawa(modelreg_reduc)
