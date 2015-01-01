############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page176.q                                                #
#                                                          #
############################################################

# run figure6-2.q first
cm30.dif <- diff(WeeklyInterest$cm30)
fit <- lm(aaa.dif ~ cm10.dif + cm30.dif, na.action=na.exclude)
anova(fit,ssType=1)
summary(fit)
summary.proc.reg(fit)

# various diagnostic plots
plot(fit)
plot(cm10.dif, cm30.dif)
plot(resid(fit))
plot(cm10.dif)
plot(cm30.dif)

