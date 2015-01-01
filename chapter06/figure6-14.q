############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-14.q                                             #
#                                                          #
############################################################

# run figure6-13.q first

# compute leverages
# leverage of x_i is x_i * inv(X'X) * x_i
# where X=gradient matrix at observations and final
# parameter values, x_i is the ith row of X
#
# for given model, gradient is exp(b0 + b1*x)*(1 x)'
#
#
X <- cbind(1, default.data$rating)
Z <- matrix(rep(exp(X %*% coef(fit.nls)),2),ncol=2) * X
#
# an object of class nls has a element called "R"
# that holds the "R" portion of a QR decomposition
# of the gradient at the final parameter values
# so X=QR => X'X = R'Q'QR = R'R since Q'Q=I. Can
# check that 
#
# abs(t(fit.nls$R) %*% fit.nls$R - t(Z) %*% Z) < small amount
#
# (the difference is due to rounding error)
#

# compute standardized residuals
fit.nls.leverages <- diag(Z %*% solve(t(fit.nls$R) %*% fit.nls$R) %*% t(Z))
fit.nls.stdres <- resid(fit.nls)/(summary(fit.nls)$sigma * sqrt(1 - fit.nls.leverages))

# convert standardized residuals to studentized residuals
# via standard trick (see Modern Applied Statistics with S, Venables
# and Ripley)
n <- nrow(default.data)
p <- 2
fit.nls.studres <- fit.nls.stdres / sqrt( (n - p - (fit.nls.stdres^2))/(n-p-1))

# note: ruppert's SAS code on page 212 uses ordinary 
# residuals, not studentized residuals

# remember the scaling of the frequencies!
# here is the ordinary version
plot( 100*predict(fit.nls), 100*abs(resid(fit.nls)), type="p",
	xlab="fitted values", ylab="absolute residual")
lines(lowess(100*predict(fit.nls), 100*abs(resid(fit.nls))))

# here is the studentized version
plot( 100*predict(fit.nls), abs(fit.nls.studres), type="p",
	xlab="fitted values", ylab="absolute residual")
lines(lowess(100*predict(fit.nls), abs(fit.nls.studres)))
	
