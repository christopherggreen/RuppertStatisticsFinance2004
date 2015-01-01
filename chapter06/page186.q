############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page186.q                                                #
#                                                          #
############################################################

ff.dif <- diff(WeeklyInterest$ff)
prime.dif <- diff(WeeklyInterest$prime)

fit1 <- lm( aaa.dif ~  cm10.dif, na.action=na.exclude)
fit2 <- lm( aaa.dif ~  cm30.dif, na.action=na.exclude)
fit3 <- lm( aaa.dif ~    ff.dif, na.action=na.exclude)
fit4 <- lm( aaa.dif ~ prime.dif, na.action=na.exclude)

fit12 <- lm( aaa.dif ~  cm10.dif +  cm30.dif, na.action=na.exclude)
fit13 <- lm( aaa.dif ~  cm10.dif +    ff.dif, na.action=na.exclude)
fit14 <- lm( aaa.dif ~  cm10.dif + prime.dif, na.action=na.exclude)
fit23 <- lm( aaa.dif ~  cm30.dif +    ff.dif, na.action=na.exclude)
fit24 <- lm( aaa.dif ~  cm30.dif + prime.dif, na.action=na.exclude)
fit34 <- lm( aaa.dif ~    ff.dif + prime.dif, na.action=na.exclude)

fit123 <- lm( aaa.dif ~  cm10.dif +  cm30.dif +    ff.dif, na.action=na.exclude)
fit124 <- lm( aaa.dif ~  cm10.dif +  cm30.dif + prime.dif, na.action=na.exclude)
fit134 <- lm( aaa.dif ~  cm10.dif +    ff.dif + prime.dif, na.action=na.exclude)
fit234 <- lm( aaa.dif ~  cm30.dif +    ff.dif + prime.dif, na.action=na.exclude)

fit1234 <- lm( aaa.dif ~ cm10.dif + cm30.dif + ff.dif + prime.dif, na.action=na.exclude)

all.fits <- list(fit1,fit2,fit3,fit4,fit12,fit13,fit14,fit23,fit24,fit34,fit123,
	fit124,fit134,fit234,fit1234)

fit.criteria <- data.frame(rep(1:4,c(4,6,4,1)),
	matrix(NA, nrow=15,ncol=5),rep(NA,15))
names(fit.criteria) <- c("Num Vars","R Sq","Adj R Sq","Cp","AIC","SBC","Variables")

# number of nonmissing observations in full model
n <- sum(!is.na(aaa.dif) & !is.na(cm10.dif) & !is.na(cm30.dif) & !is.na(ff.dif) & !is.na(prime.dif))

# R-Squared values
fit.criteria[,"R Sq"] <- sapply(all.fits, function(x) summary(x)$r.squared)

# Adjusted R-Squared values
fit.criteria[,"Adj R Sq"] <- 1 - (n/(n-fit.criteria[,"Num Vars"]))*(1-fit.criteria[,"R Sq"])

# mean squared errors
mse.list <- sapply(all.fits, function(x) {y<-anova(x);y[nrow(y),"Mean Sq"]})

# Mallow's Cp
fit.criteria[,"Cp"] <- sapply(all.fits, function(x) {y<-anova(x);y[nrow(y),"Sum of Sq"]})
fit.criteria[,"Cp"] <- (fit.criteria[,"Cp"]/mse.list[[length(mse.list)]]) - n
fit.criteria[,"Cp"] <- 	fit.criteria[,"Cp"] + 2*(fit.criteria[,"Num Vars"]+1)

# AIC
fit.criteria[,"AIC"] <- n*sapply(mse.list, log) + 2*(fit.criteria[,"Num Vars"]+1)

# SBC
fit.criteria[,"SBC"] <- n*sapply(mse.list, log) + log(n)*(fit.criteria[,"Num Vars"]+1)

# variable names
fit.criteria[,"Variables"] <- sapply(all.fits, 
	function(x) paste(attr(x$terms,"term.labels"), collapse=" "))
	
by(fit.criteria, fit.criteria$"Num Vars", function(x)x)
