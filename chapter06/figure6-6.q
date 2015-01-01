############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-6.q                                              #
#                                                          #
############################################################

# create a "fake" version of WeeklyInterest, in which missing
# values are mistaken for actual zeros
#
WeeklyInterest.fake <- WeeklyInterest
WeeklyInterest.fake$cm30[which.na(WeeklyInterest.fake$cm30)] <- 0
aaa.dif.fake <- diff(WeeklyInterest.fake$aaa)
cm10.dif.fake <- diff(WeeklyInterest.fake$cm10)
cm30.dif.fake <- diff(WeeklyInterest.fake$cm30)

incorrect.fit <- lsfit(cbind(cm10.dif.fake,cm30.dif.fake),aaa.dif.fake)
incorrect.fit.diagnostics <- ls.diag(incorrect.fit)

par(mfrow=c(2,2),lwd=3,las=1)
plot(incorrect.fit.diagnostics$hat, type="l", xlab="case number", ylab="leverage")
plot(incorrect.fit.diagnostics$stud.res, type="l", xlab="case number", ylab="")
plot(incorrect.fit.diagnostics$cooks, type="p", xlab="case number", ylab="Cook's D")
par(mfrow=c(1,1))
