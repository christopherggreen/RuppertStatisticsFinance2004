############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-20.q                                             #
#                                                          #
############################################################

n <- 22
x <- y <- rep(NA,n)
x[-n] <- seq(0,10,length=n-1)
y[-n] <- 3+2*x[-n] + rnorm(n-1,0,0.1)
x[n] <- 2
y[n] <- 17

# fit with outlier
fit.w  <- lm(y ~ x)
# fit without outlier
fit.wo <- lm(y[-n] ~ x[-n])

plot( x, y )
abline( coef(fit.wo) )
abline( coef(fit.w ), lty=3)

text(3.1,19.0,"residual outlier")
text(3.0, 4.0,"w/o outlier")
text(0.8,12.0,"w outlier")

arrows( 3.1, 19.0, 2, 17, lty=1)
arrows( 3.0, 4.0, 3.5, 10, lty=1)
arrows( 0.8, 12.0, 1.5, 
	sum(c(1,1.5)*coef(fit.w)), lty=1)

