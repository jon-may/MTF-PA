
library("lavaan")
library("semTools")
MTFPA1 <- read_csv("http://github.com/jon-may/MTF-PA/raw/master/St1MTFnoout.csv")


#  sink('C:/TEMP/MTFnooutput1.txt',split=TRUE)

MTF.model1 <- 'intensity =~ WANT + NEED + URGE + GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST + THOUGHT + REMIND + GRAB'


MTF.model2a <- 'intensity =~ WANT + NEED + URGE
imagavail =~ GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST + THOUGHT + REMIND + GRAB'

MTF.model2b <- 'intenimag =~ WANT + NEED + URGE + GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST
availability =~ THOUGHT + REMIND + GRAB'

MTF.model2c <- 'intenavail =~ WANT + NEED + URGE + THOUGHT + REMIND + GRAB
imagery =~ GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST'

MTF.model3 <- 'intensity =~ WANT + NEED + URGE 
imagery =~ GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST
availability =~ THOUGHT + REMIND + GRAB'

MTF.model4 <- 'intensity =~ WANT + NEED + URGE
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST
availability =~ THOUGHT + REMIND + GRAB'


fit <- cfa(MTF.model1, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)

fit <- cfa(MTF.model2a, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model2b, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model2c, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model3, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(MTF.model4, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)

MTF.model4e1 <- 'intensity =~ WANT + NEED + URGE
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST
availability =~ THOUGHT + REMIND + GRAB
DOING ~~ HOW '

fit <- cfa(MTF.model4e1, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e2 <- 'intensity =~ WANT + NEED + URGE
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST
availability =~ THOUGHT + REMIND + GRAB
DOING ~~ HOW 
WANT ~~ URGE'

fit <- cfa(MTF.model4e2, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)

MTF.model4e3 <- 'intensity =~ WANT + NEED + URGE
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST
availability =~ THOUGHT + REMIND + GRAB
DOING ~~ HOW 
WANT ~~ URGE
REMIND ~~ GRAB'

fit <- cfa(MTF.model4e3, data=MTFPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)


fit <- cfa(MTF.model4e3, data=MTFPA1, estimator="MLR", group = "site") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
measurementInvariance(MTF.model4e1, data=MTFPA1, estimator="MLR", group = "site")
reliability(fit)


# sink()

