### Analysis of Study 1 using MTFSMPA.csv
### NB set your working directory to the folder containing this script and the data file
# setwd("/R_Analysis")

## load packages
library(readr)
library("lavaan")
library("semTools")
library(tidyverse)
library(MBESS)
library(numform)

## function to format decimal places and drop leading zero

dp<-function(x,n){
  return(as.numeric(f_num(x,n,zero=NULL)))
}

## CFA anlayses are long so increase maximum number of lines to print
options(max.print = 99999)

## read data file
MTFPA1 <- read.csv("MTFSMPA.csv")

# count participants by Sex
MTFPA1$Sex<-as.factor(MTFPA1$Sex)
MTFPA1$Sex<-recode_factor(MTFPA1$Sex, '1' = "Male",
                          '2' = "Female")
MTFPA1$Site<-as.factor(MTFPA1$Site)
MTFPA1$Site<-recode_factor(MTFPA1$Site, '1' = "Aus",
                          '2' = "UK")
table(MTFPA1$Sex, MTFPA1$Site)
chisq.test(table(MTFPA1$Sex, MTFPA1$Site))


# compare ages by site
t.test(MTFPA1$Age ~ MTFPA1$Site)

# count participants by Relationship
MTFPA1$Relationship<-as.factor(MTFPA1$Relationship)
MTFPA1$Relationship<-recode_factor(MTFPA1$Relationship, '1' = "single",
                                '2' = "in relationship",
                                '3' = "married/de facto",
                                '4' = "separated",
                                '5' = "divorced",
                                '6' = "widowed")
table(MTFPA1$Relationship, MTFPA1$Site)
chisq.test(table(MTFPA1$Relationship, MTFPA1$Site))

# count participants by Education
MTFPA1$Education<-as.factor(MTFPA1$Education)
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

fit1 <- cfa(MTF.model1, data=MTFPA1, estimator="MLR") 
fitMeasures(fit1, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)


## fit the 3 Factor model
fit3 <- cfa(MTF.model3, data=MTFPA1, estimator="MLR") 
fitMeasures(fit3, fit.scaled)
#summary(fit, fit.measures=TRUE, modindices=TRUE)

## fit the 4 Factor model
fit4 <- cfa(MTF.model4, data=MTFPA1, estimator="MLR") 
fitMeasures(fit4, fit.scaled)
summary(fit4, fit.measures=TRUE, modindices=TRUE)
reliability(fit4)

## reduce dataframe to MTF subscales for use in MBESS functions
MTF.intensity <-MTFPA1 %>% select(WANTF,NEEDF,URGEF) %>% drop_na(WANTF,NEEDF,URGEF)
MTF.imageryi <-MTFPA1 %>% select(GOODF, BETTERF, WORSEF) %>% drop_na(GOODF, BETTERF, WORSEF)
MTF.imageryse <-MTFPA1 %>% select(DOINGF, HOWF, SUCCEEDF, PASTF) %>% drop_na(DOINGF, HOWF, SUCCEEDF, PASTF)
MTF.availability <-MTFPA1 %>% select(THOUGHTF, REMINDF, GRABF) %>% drop_na(THOUGHTF, REMINDF, GRABF)
MTF.scale <- MTFPA1 %>% select(WANTF:GRABF)%>% drop_na(WANTF:GRABF)

#compute reliability CIs for each scale
# these take a long time to return if B=10000 so reduce to 100 to test
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
# using deprecated semTools function
measurementInvariance(model=MTF.model4, data=MTFPA1, estimator="MLR", group = "Site")
# by hand


fit.metricModel <- 'intensity =~ c(NA, NA)*WANTF + NEEDF + URGEF ## freely estimate 1st factor loading in both sites
                     imageryi =~ c(NA, NA)*GOODF + BETTERF + WORSEF 
                    imageryse =~ c(NA, NA)*DOINGF + HOWF + SUCCEEDF + PASTF
                 availability =~ c(NA, NA)*THOUGHTF + REMINDF + GRABF
                    intensity =~ c(NA, 1)*intensity ## fix the factor variance to 1 in 2nd site, and freely estimate in 1st site
                     imageryi =~ c(NA, 1)*imageryi
                    imageryse =~ c(NA, 1)*imageryse
                 availability =~ c(NA, 1)*availability
                    intensity =~ c(0, 0 )*1
                     imageryi =~ c(0, 0 )*1
                    imageryse =~ c(0, 0 )*1
                 availability =~ c(0, 0 )*1'

fit.scalarModel <- 'intensity =~ c(NA, NA)*WANTF + NEEDF + URGEF ## freely estimate 1st factor loading in both sites
                     imageryi =~ c(NA, NA)*GOODF + BETTERF + WORSEF 
                    imageryse =~ c(NA, NA)*DOINGF + HOWF + SUCCEEDF + PASTF
                 availability =~ c(NA, NA)*THOUGHTF + REMINDF + GRABF
                    intensity =~ c(NA, 1)*intensity ## fix the factor variance to 1 in 2nd site, and freely estimate in 1st site
                     imageryi =~ c(NA, 1)*imageryi
                    imageryse =~ c(NA, 1)*imageryse
                 availability =~ c(NA, 1)*availability
                    intensity =~ c(NA, 0 )*1
                     imageryi =~ c(NA, 0 )*1
                    imageryse =~ c(NA, 0 )*1
                 availability =~ c(NA, 0 )*1'
  

fit.config <- cfa(model=MTF.model4, data=MTFPA1, std.lv=TRUE, estimator="MLR", group = "Site") 
fit.metric <- cfa(model=MTF.model4, data=MTFPA1, estimator="MLR", group = "Site", 
                  group.equal="loadings") 
fit.scalar <- cfa(model=MTF.model4, data=MTFPA1, estimator="MLR", group = "Site", 
                  group.equal = c("loadings", "intercepts")) 
fit.strict <- cfa(model=MTF.model4, data=MTFPA1, estimator="MLR", group = "Site", 
                  group.equal = c("loadings", "intercepts", "residuals")) 
fit.means <- cfa(model=MTF.model4, data=MTFPA1, estimator="MLR", group = "Site", 
                 group.equal = c("loadings", "intercepts", "residuals", "means")) 

anova(fit.config, fit.metric, fit.scalar, fit.strict, fit.means)

fitMeasures(fit.config, fit.scaled)
fitMeasures(fit.metric, fit.scaled)
fitMeasures(fit.scalar, fit.scaled)
fitMeasures(fit.strict, fit.scaled)
fitMeasures(fit.means, fit.scaled)

fitMeasures(fit.config, "cfi")
fitMeasures(fit.metric, "cfi")
fitMeasures(fit.scalar, "cfi")
fitMeasures(fit.strict, "cfi")
fitMeasures(fit.means, "cfi")

fitMeasures(fit.metric, "cfi")-fitMeasures(fit.config, "cfi")
fitMeasures(fit.scalar, "cfi")-fitMeasures(fit.metric, "cfi")
fitMeasures(fit.strict, "cfi")-fitMeasures(fit.scalar, "cfi")
fitMeasures(fit.means, "cfi")-fitMeasures(fit.strict, "cfi")


# using new measEq.syntax function of semTools


fit.config<-measEq.syntax(configural.model = MTF.model4, data = MTFPA1,
                          parameterization = "theta",
                          ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                          group = "Site", 
                          return.fit = TRUE)

fit.metric<-measEq.syntax(configural.model = MTF.model4, data = MTFPA1,
                          parameterization = "theta",
                          ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                          group = "Site", 
                          group.equal = c("thresholds","loadings"),
                          return.fit = TRUE)

fit.scalar<-measEq.syntax(configural.model = MTF.model4, data = MTFPA1,
                          parameterization = "theta",
                          ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                          group = "Site", 
                          group.equal = c("thresholds","loadings","intercepts"),
                          return.fit = TRUE)

fit.scalar<-measEq.syntax(configural.model = MTF.model4, data = MTFPA1,
                          parameterization = "theta",
                          ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                          group = "Site", 
                          group.equal = c("thresholds","loadings","intercepts"),
                          return.fit = TRUE)

fit.strict<-measEq.syntax(configural.model = MTF.model4, data = MTFPA1,
                          parameterization = "theta",
                          ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                          group = "Site", 
                          group.equal = c("thresholds","loadings","intercepts","residuals"),
                          return.fit = TRUE)


fit.means<-measEq.syntax(configural.model = MTF.model4, data = MTFPA1,
                          parameterization = "theta",
                          ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                          group = "Site", 
                          group.equal = c("thresholds","loadings","intercepts","residuals","means"),
                          return.fit = TRUE)




cfi.table<-rbind(Config= c(dp(fitMeasures(fit.config, "cfi"),3),NULL),
                 Metric= c(dp(fitMeasures(fit.metric, "cfi"),3),dp(fitMeasures(fit.metric, "cfi")-fitMeasures(fit.config, "cfi"),3)),
                 Scalar=c(dp(fitMeasures(fit.scalar, "cfi"),3),dp(fitMeasures(fit.scalar, "cfi")-fitMeasures(fit.metric, "cfi"),3)),
                 Strict=c(dp(fitMeasures(fit.strict, "cfi"),3),dp(fitMeasures(fit.strict, "cfi")-fitMeasures(fit.scalar, "cfi"),3)),
                  Means=c(dp(fitMeasures(fit.means, "cfi"),3),dp(fitMeasures(fit.means, "cfi")-fitMeasures(fit.strict, "cfi"),3)))
colnames(cfi.table)<-c("CFI","Delta")

anova(fit.config,fit.metric, fit.scalar, fit.strict, fit.means)
cfi.table
## close output file
sink()





