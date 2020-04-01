### Analysis o Study 2-snacking using HugoDall.csv
### NB set your working directory to the folder containing this script and the data file
# setwd("/R_Analysis")

## load packages
library(tidyverse)
library("readr")
library("lavaan")
library("semTools")
library("psych")
library("MBESS")

## CFA anlayses are long so increase maximum number of lines to print
options(max.print = 99999)

## read data file
Study2S <- read.csv("Study2S.csv")

## create output file for analysis to be sent to
sink('Study2.txt',split=TRUE)


## make a list of fit measures to report
fit.scaled<-c("chisq.scaled", "df", "cfi.robust", "tli.robust", "aic", "bic", "srmr", "srmr_bentler", "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust")





## define three models to compare
## Unifactorial model as a baseline
MTF.model1 <- 'intensity =~ WANT  + NEED  + URGE  + GOOD  + BETTER  + WORSE  + DOING  + HOW  + SUCCEED  + PAST  + THOUGHT  + REMIND  + GRAB '

## three  actor model as in Robinson et al. (2016)
MTF.model3 <- 'intensity =~ WANT  + NEED  + URGE  
imagery =~ GOOD  + BETTER  + WORSE  + DOING  + HOW  + SUCCEED  + PAST 
availability =~ THOUGHT  + REMIND  + GRAB '

## four factor model as in Parham et al (2016) and Kavanagh et al (2018)
MTF.model4 <- 'intensity =~ WANT  + NEED  + URGE 
imageryi =~ GOOD  + BETTER  + WORSE  
imageryse =~ DOING  + HOW  + SUCCEED  + PAST 
availability =~ THOUGHT  + REMIND  + GRAB '

# fit the models and generate summary fit measures
fit <- cfa(MTF.model1, data=Study2S, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model3, data=Study2S, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model4, data=Study2S, estimator="MLR") 
fitMeasures(fit, fit.scaled)
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

## reduce dataframe to MTF subscales for use in MBESS functions
MTF.intensity <-Study2S %>% select(WANT,NEED,URGE) %>% drop_na(WANT,NEED,URGE)
MTF.imageryi <-Study2S %>% select(GOOD, BETTER, WORSE) %>% drop_na(GOOD, BETTER, WORSE)
MTF.imageryse <-Study2S %>% select(DOING, HOW, SUCCEED, PAST) %>% drop_na(DOING, HOW, SUCCEED, PAST)
MTF.availability <-Study2S %>% select(THOUGHT, REMIND, GRAB) %>% drop_na(THOUGHT, REMIND, GRAB)
MTF.scale <- Study2S %>% select(WANT:GRAB)%>% drop_na(WANT:GRAB)

# compute reliability CIs for each scale
# functions take a long tie to return woith B=10000 so reduce to B=100 to test
twodp<-function(x){return(format(round(x, 2), nsmall = 2))}




ci<-ci.reliability(data=MTF.intensity, 
                   type="hierarchical", conf.level=.95, B=10000, interval.type="perc")
paste0("intensity: ",twodp(ci$est)," [",twodp(ci$ci.lower),", ",twodp(ci$ci.upper),"]")

ci<-ci.reliability(data=MTF.imageryi, 
                   type="hierarchical", conf.level=.95, B=10000, interval.type="perc")
paste0("imageryi: ",twodp(ci$est)," [",twodp(ci$ci.lower),", ",twodp(ci$ci.upper),"]")

ci<-ci.reliability(data=MTF.imageryse, 
                   type="hierarchical", conf.level=.95, B=10000, interval.type="perc")
paste0("imageryse: ",twodp(ci$est)," [",twodp(ci$ci.lower),", ",twodp(ci$ci.upper),"]")

ci<-ci.reliability(data=MTF.availability, 
                   type="hierarchical", conf.level=.95, B=10000, interval.type="perc")
paste0("availability: ",twodp(ci$est)," [",twodp(ci$ci.lower),", ",twodp(ci$ci.upper),"]")

ci<-ci.reliability(data=MTF.scale, 
                   type="hierarchical", conf.level=.95, B=10000, interval.type="perc")
paste0("total: ",twodp(ci$est)," [",twodp(ci$ci.lower),", ",twodp(ci$ci.upper),"]")

## iteratively add highest MI error covariance pair within factor

MTF.model4e1 <- 'intensity =~ WANT + NEED + URGE 
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST 
availability =~ THOUGHT + REMIND + GRAB 
DOING ~~ HOW '
fit <- cfa(MTF.model4e1, data=Study2S, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e2 <- 'intensity =~ WANT + NEED + URGE 
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST 
availability =~ THOUGHT + REMIND + GRAB 
DOING ~~ HOW 
WANT ~~ URGE '
fit <- cfa(MTF.model4e2, data=Study2S, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e3 <- 'intensity =~ WANT + NEED + URGE 
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST 
availability =~ THOUGHT + REMIND + GRAB 
DOING ~~ HOW 
WANT ~~ URGE 
REMIND ~~ GRAB '
fit <- cfa(MTF.model4e3, data=Study2S, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)


# close output file
sink()



