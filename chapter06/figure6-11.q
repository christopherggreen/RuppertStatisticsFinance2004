############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-11.q                                             #
#                                                          #
############################################################

# run figure 6-8.q first
fit.predicted <- as.vector(cbind(1,x1,x2) %*% fit$coef)
plot(fit.predicted, abs(fit.diag$stud.res), las=1, type="p", 
	xlab="predicted values", ylab="std. residuals")
# add spline smoother
lines(lowess(fit.predicted,abs(fit.diag$stud.res)))


