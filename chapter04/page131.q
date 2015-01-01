############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page131.q                                                #
#                                                          #
############################################################

logR.forecast.mle <- arima.forecast(logR, model=list(order=c(1,0,0)), n=20)
# figure 4.8
plot(logR, type="l", xlim=c(0,300), ylab="log return", xlab="time")
lines(length(logR)+seq(1,20), logR.forecast.mle$mean, lwd=2)
lines(length(logR)+seq(1,20), 
	logR.forecast.mle$mean - 1.96*logR.forecast.mle$std.err, lwd=2, lty=3)
lines(length(logR)+seq(1,20), 
	logR.forecast.mle$mean + 1.96*logR.forecast.mle$std.err, lwd=2, lty=3)

logP.forecast.mle <- arima.forecast(logP, model=list(order=c(1,1,0)), n=20)
# figure 4.9
plot(logP, type="l", xlim=c(0,300), ylab="log price", xlab="time")
lines(length(logP)+seq(1,20), logP.forecast.mle$mean, lwd=2)
lines(length(logP)+seq(1,20), 
	logP.forecast.mle$mean - 1.96*logP.forecast.mle$std.err, lwd=2, lty=3)
lines(length(logP)+seq(1,20), 
	logP.forecast.mle$mean + 1.96*logP.forecast.mle$std.err, lwd=2, lty=3)
