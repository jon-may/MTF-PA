### Analysis of Study 2 using MTFSMPA.csv
### NB set your working directory to the folder containing this script and the data file
# setwd("/R_Analysis")

## load packages
library("readr")
library("lavaan")
library("semTools")
library("psych")

## CFA anlayses are long so increase maximum number of lines to print
options(max.print = 99999)

## read data file
MTFPA2 <- read.csv("Study2.csv")

## create output file for analysis to be sent to
sink('Study2.txt',split=TRUE)


## make a list of fit measures to report
fit.scaled<-c("chisq.scaled", "df", "cfi.robust", "tli.robust", "aic", "bic", "srmr", "srmr_bentler", "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust")





## define three models to compare
## Unifactorial model as a baseline
MTF.model1 <- 'intensity =~ WANTF + NEEDF + URGEF + GOODF + BETTERF + WORSEF + DOINGF + HOWF + SUCCEEDF + PASTF + THOUGHTF + REMINDF + GRABF'

## three factor model as in Robinson et al. (2016)
MTF.model3 <- 'intensity =~ WANTF + NEEDF + URGEF 
imagery =~ GOODF + BETTERF + WORSEF + DOINGF + HOWF + SUCCEEDF + PASTF
availability =~ THOUGHTF + REMINDF + GRABF'

## four factor model as in Parham et al (2016) and Kavanagh et al (2018)
MTF.model4 <- 'intensity =~ WANTF + NEEDF + URGEF
imageryi =~ GOODF + BETTERF + WORSEF 
imageryse =~ DOINGF + HOWF + SUCCEEDF + PASTF
availability =~ THOUGHTF + REMINDF + GRABF'

# fit the models and generate summary fit measures
fit <- cfa(MTF.model1, data=MTFPA2, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model3, data=MTFPA2, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model4, data=MTFPA2, estimator="MLR") 
fitMeasures(fit, fit.scaled)
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)


## iteratively add highest MI error covariance pair within factor

MTF.model4e1 <- 'intensity =~ WANTF + NEEDF + URGEF
imageryi =~ GOODF + BETTERF + WORSEF 
imageryse =~ DOINGF + HOWF + SUCCEEDF + PASTF
availability =~ THOUGHTF + REMINDF + GRABF
DOINGF ~~ HOWF '
fit <- cfa(MTF.model4e1, data=MTFPA2, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e2 <- 'intensity =~ WANTF + NEEDF + URGEF
imageryi =~ GOODF + BETTERF + WORSEF 
imageryse =~ DOINGF + HOWF + SUCCEEDF + PASTF
availability =~ THOUGHTF + REMINDF + GRABF
DOINGF ~~ HOWF 
WANTF ~~ URGEF'
fit <- cfa(MTF.model4e2, data=MTFPA2, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e3 <- 'intensity =~ WANTF + NEEDF + URGEF
imageryi =~ GOODF + BETTERF + WORSEF 
imageryse =~ DOINGF + HOWF + SUCCEEDF + PASTF
availability =~ THOUGHTF + REMINDF + GRABF
DOINGF ~~ HOWF 
WANTF ~~ URGEF
REMINDF ~~ GRABF'
fit <- cfa(MTF.model4e3, data=MTFPA2, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)


# close output file
sink()

