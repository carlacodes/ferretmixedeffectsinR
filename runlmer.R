## load lme4, JWileymisc, and multilevelTools packages
## (i.e., "open the 'apps' ") 
library(lme4)
#> Loading required package: Matrix
library(lmerTest)
#> 
#> Attaching package: 'lmerTest'
#> The following object is masked from 'package:lme4':
#> 
#>     lmer
#> The following object is masked from 'package:stats':
#> 
#>     step
library(extraoperators)
library(JWileymisc)
library(multilevelTools)

dfuse <- read.csv("dfuse.csv")
dfcatuse <- read.csv("dfcat_use.csv")

df <- read.csv("df.csv")
dfcat <- read.csv("dfcat.csv")

modelreg_reduc <- lmer(
  "realRelReleaseTimes ~ talker*(pitchoftarg)+side + talker*stepval+timeToTarget  + (1|ferret)",
  data=dfuse, family='gamma')
