### Analysis of Study 1 using MTFSMPA.csv
### NB set your working directory to the folder containing this script and the data file
# setwd("/R_Analysis")

## load packages
library(readr)
library("lavaan")
library("semTools")
library(tidyverse)

## CFA anlayses are long so increase maximum number of lines to print
options(max.print = 99999)

## read data file
MTFPA1 <- read.csv("MTFSMPA.csv")

# count participants by Sex
as.factor(MTFPA1$Sex)
MTFPA1$Sex<-recode_factor(MTFPA1$Sex, '1' = "Male",
                          '2' = "Female")
table(MTFPA1$Sex, MTFPA1$Site)
chisq.test(table(MTFPA1$Sex, MTFPA1$Site))


# compare ages by site
t.test(MTFPA1$Age ~ MTFPA1$Site)

# count participants by Relationship
as.factor(MTFPA1$Relationship)
MTFPA1$Relationship<-recode_factor(MTFPA1$Relationship, '1' = "single",
                                '2' = "in relationship",
                                '3' = "married/de facto",
                                '4' = "separated",
                                '5' = "divorced",
                                '6' = "widowed")
table(MTFPA1$Relationship, MTFPA1$Site)
chisq.test(table(MTFPA1$Relationship, MTFPA1$Site))

# count participants by Education
as.factor(MTFPA1$Education)
MTFPA1$Education<-recode_factor(MTFPA1$Education, '1' = "Secondary",
                                '2' = "trade, diploma, certificate",
                                '3' = "currently undergraduate",
                                '4' = "completed undergraduate",
                                '5' = "completed postgraduate")
table(MTFPA1$Education, MTFPA1$Site)
chisq.test(table(MTFPA1$Education, MTFPA1$Site))



## create output file for analysis to be sent to
sink('MTFAnalysis.txt', split='TRUE')

fit.scaled<-c("chisq.scaled", "df", "cfi.robust", "tli.robust", "aic", "bic", "srmr", "srmr_bentler", "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust")


## define three models to compare
## Unifactorial model as a baseline
MTF.model1 <- 'intensity =~ WANTF + NEEDF + URGEF + 
            GOODF + BETTERF + WORSEF + DOINGF + HOWF + SUCCEEDF + PASTF + 
            THOUGHTF + REMINDF + GRABF'

## three factor model as in Robinson et al. (2016)
MTF.model3 <- 'intensity =~ WANTF + NEEDF + URGEF 
             imagery =~ GOODF + BETTERF + WORSEF + DOINGF + HOWF + SUCCEEDF + PASTF
             availability =~ THOUGHTF + REMINDF + GRABF'

## four factor model as in Parham et al (2016) and Kavanagh et al (2018)
MTF.model4 <- 'intensity =~ WANTF + NEEDF + URGEF
               imageryi =~ GOODF + BETTERF + WORSEF 
              imageryse =~ DOINGF + HOWF + SUCCEEDF + PASTF
            availability =~ THOUGHTF + REMINDF + GRABF'

## fit the Unifactorial model

fit <- cfa(MTF.model1, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)


## fit the 3 Factor model
fit <- cfa(MTF.model3, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)

## fit the 4 Factor model
fit <- cfa(MTF.model4, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

## iteratively add highest MI error covariance pair within factor
MTF.model4e1 <- paste(MTF.model4,"\nDOINGF ~~ HOWF")
fit <- cfa(MTF.model4e1, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e2 <- paste(MTF.model4e1,'\n WANTF ~~ URGEF')
fit <- cfa(MTF.model4e2, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)


MTF.model4e3 <- paste(MTF.model4e2,'\n REMINDF ~~ GRABF')
fit <- cfa(MTF.model4e3, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

fitMeasures(fit, fit.scaled)

MTF.model4e4 <- paste(MTF.model4e3,'\n GOODF ~~ WORSEF')
fit <- cfa(MTF.model4e4, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e5 <- paste(MTF.model4e4,'\n SUCCEEDF ~~ PASTF')
fit <- cfa(MTF.model4e5, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

## return to model4 to compare invariances over site
fit <- cfa(MTF.model4, data=MTFPA1, estimator="MLR", group = "Site") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
measurementInvariance(MTF.model4, data=MTFPA1, estimator="MLR", group = "Site")
reliability(fit)


## try again with the four EV added
fit <- cfa(MTF.model4e4, data=MTFPA1, estimator="MLR", group = "Site") 
fitMeasures(fit, fit.scaled)
summary(fit, fit.measures=TRUE, modindices=TRUE)
measurementInvariance(MTF.model4e4, data=MTFPA1, estimator="MLR", group = "Site")
reliability(fit)

## close output file
sink()

### Analyses for SM

## open output file
sink('SMAnalysis.txt',split=TRUE)

## One factor for baseline
SMS.model1a <- 'intensity =~ WANT + NEED + URGE + 
            GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST + 
            REMIND + GRAB '
fit <- cfa(SMS.model1a, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)


## 3 factor as in Robinson et al. (2016)
SMS.model3a <- 'intensity =~ WANT + NEED + URGE 
                imagery =~ GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST
                 availability =~ REMIND + GRAB'
fit <- cfa(SMS.model3a, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)


## 4 factor as in Parham (2016), Kavanagh (2018)
SMS.model4a <- 'intensity =~ WANT + NEED + URGE
                imageryi =~ GOOD + BETTER + WORSE 
                imageryse =~ DOING + HOW + SUCCEED + PAST
                availability =~ REMIND + GRAB'
fit <- cfa(SMS.model4a, data=MTFPA1, estimator="MLR") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

## check invariance over sites
fit <- cfa(SMS.model4a, data= MTFPA1, estimator="MLR", group = "Site") 
fitMeasures(fit, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)
measurementInvariance(SMS.model4a, data= MTFPA1, estimator="MLR", group = "Site")
reliability(fit) 

sink()



