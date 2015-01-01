############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-8.q                                              #
#                                                          #
############################################################

# run figure6-7.q first

# fit model
fit <- lsfit(cbind(x1,x2),y)
fit.diag <- ls.diag(fit)

par(mfrow=c(2,1),las=1,lwd=3)
qqplot.matlab(fit.diag$stud.res, xlabels=seq(-2,9), 
	qlabels=c(0.01,0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98,0.99),
	xlab="Std. residual")
hist(fit.diag$stud.res, xlab="Std. residual", ylab="count")
par(mfrow=c(1,1))
