library("lavaan")
library("semTools")
SMPA1 <- read_csv("http://github.com/jon-may/MTF-PA/raw/master/St1SMnoout.csv")

#sink('C:/TEMP/SMPANOOUTput.txt',split=TRUE)

SMS.model1 <- 'intensity =~ WANT + NEED + URGE + GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST + REMIND + GRAB + KEEP'


SMS.model2a <- 'intensity =~ WANT + NEED + URGE
imagavail =~ GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST + REMIND + GRAB + KEEP'

SMS.model2b <- 'intenimag =~ WANT + NEED + URGE + GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST
availability =~ REMIND + GRAB + KEEP'

SMS.model2c <- 'intenavail =~ WANT + NEED + URGE + REMIND + GRAB + KEEP
imagery =~ GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST'

SMS.model3 <- 'intensity =~ WANT + NEED + URGE 
imagery =~ GOOD + BETTER + WORSE + DOING + HOW + SUCCEED + PAST
availability =~ REMIND + GRAB + KEEP'

SMS.model4 <- 'intensity =~ WANT + NEED + URGE
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST
availability =~ REMIND + GRAB + KEEP'


fit <- cfa(SMS.model1, data=SMPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)

fit <- cfa(SMS.model2a, data=SMPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(SMS.model2b, data=SMPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(SMS.model2c, data=SMPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(SMS.model3, data=SMPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
fit <- cfa(SMS.model4, data=SMPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)


SMS.model4e1 <- 'intensity =~ WANT + NEED + URGE
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST
availability =~ REMIND + GRAB + KEEP
REMIND ~~ GRAB '

fit <- cfa(SMS.model4e1, data=SMPA1, estimator="MLR") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)


SMS.model4e1 <- 'intensity =~ WANT + NEED + URGE
imageryi =~ GOOD + BETTER + WORSE 
imageryse =~ DOING + HOW + SUCCEED + PAST
availability =~ REMIND + GRAB + KEEP
REMIND ~~ GRAB '

fit <- cfa(SMS.model4e1, data=SMPA1, estimator="MLR", group = "site") 
summary(fit, fit.measures=TRUE, modindices=TRUE)
reliability(fit)
measurementInvariance(SMS.model4e1, data=SMPA1, estimator="MLR", group = "site")


#sink()
