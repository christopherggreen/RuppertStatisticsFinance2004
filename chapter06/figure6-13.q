############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-13.q                                             #
#                                                          #
############################################################

# run figure6-12.q first

# the BOW procedure
rating.bow <- default.data$rating
# set zero values to missing, then exclude them in the regression
rating.bow[default.data$frequency==0] <- NA
fit.bow <- lm( log(default.data$frequency) ~ rating.bow, 
	na.action=na.exclude)

par(xaxs="i",yaxs="i",las=1, lwd=3)
plot( default.data$rating, log10(100*default.data$frequency + 1e-5), 
	type="p", xlab="rating", ylab="default probability", axes=F,
	ylim=c(-5,2))
axis(1, at=default.data$rating)
axis(2, at=seq(-5,2), labels=paste("10^",seq(-5,2)), adj=1)
box()

# modify bow results for plotting
# BOW model was fit on natural log scale, transform 
# to log10 scale. also adjust intercept by 100 to 
# transform back to percentage scale
abline(c(2,0)+log10(exp(1))*coef(fit.bow), lty=3)

# nls results
lines( default.data$rating, log10(100*predict(fit.nls)), lty=5 )

# tbs results

key(corner=c(1,0), border=T,
	text=list(c("data","BOW","nonlinear", "TBS")),
	lines=list(lty=c(0,3,5,1)),
	points=list(pch=c(1,0,0,0)))
