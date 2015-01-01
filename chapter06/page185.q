############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page185.q                                                #
#                                                          #
############################################################

# fit models
fit1 <- lm(aaa.dif ~ cm10.dif, na.action=na.exclude)
fit2 <- lm(aaa.dif ~ cm30.dif, na.action=na.exclude)
fit3 <- lm(aaa.dif ~ cm10.dif + cm30.dif, na.action=na.exclude)

# compute selection criteria
fit.criteria <- data.frame(c(1,1,2),matrix(NA, nrow=3,ncol=5),c("cm10.dif","cm30.dif","cm10.dif cm30.dif"))
names(fit.criteria) <- c("Num Vars","R Sq","Adj R Sq","Cp","AIC","SBC","Variables")

# number of nonmissing observations in full model
n <- sum(!is.na(aaa.dif) & !is.na(cm10.dif) & !is.na(cm30.dif))

# R-Squared values
fit.criteria[1,"R Sq"] <- summary(fit1)$r.squared
fit.criteria[2,"R Sq"] <- summary(fit2)$r.squared
fit.criteria[3,"R Sq"] <- summary(fit3)$r.squared

# Adjusted R-Squared values
fit.criteria[1,"Adj R Sq"] <- 1 - (n/(n-1))*(1-fit.criteria[1,"R Sq"])
fit.criteria[2,"Adj R Sq"] <- 1 - (n/(n-1))*(1-fit.criteria[2,"R Sq"])
fit.criteria[3,"Adj R Sq"] <- 1 - (n/(n-2))*(1-fit.criteria[3,"R Sq"])

# mean squared errors
mse1 <- anova(fit1)[2,"Mean Sq"]
mse2 <- anova(fit2)[2,"Mean Sq"]
mse3 <- anova(fit3)[3,"Mean Sq"]

# Mallow's Cp
fit.criteria[1,"Cp"] <- anova(fit1)[2,"Sum of Sq"]/mse3 - n + 2*(1+1)
fit.criteria[2,"Cp"] <- anova(fit2)[2,"Sum of Sq"]/mse3 - n + 2*(1+1)
fit.criteria[3,"Cp"] <- anova(fit3)[3,"Sum of Sq"]/mse3 - n + 2*(2+1)

# AIC
fit.criteria[1,"AIC"] <- n*log(mse1) + 2*(1+1)
fit.criteria[2,"AIC"] <- n*log(mse2) + 2*(1+1)
fit.criteria[3,"AIC"] <- n*log(mse3) + 2*(2+1)

# SBC
fit.criteria[1,"SBC"] <- n*log(mse1) + log(n)*(1+1)
fit.criteria[2,"SBC"] <- n*log(mse2) + log(n)*(1+1)
fit.criteria[3,"SBC"] <- n*log(mse3) + log(n)*(2+1)

# print out fit.criteria, but separated by the number of variables in the model
by(fit.criteria, fit.criteria$"Num Vars", function(x)x)
