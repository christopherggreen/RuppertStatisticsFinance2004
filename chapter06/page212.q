############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page212.q                                                #
#                                                          #
############################################################

# this part was done previously in figure6-12.q
# not necessary to do it again, but we'll include it for
# completeness
# change FALSE to TRUE to run it
#
if ( FALSE ) {
	default.data <- read.table("../common/data/DefaultData.txt", 
		skip=4, header=F)
	names(default.data) <- c("rating","frequency")
	# frequencies are expressed as percentages
	# for modeling we need them expressed as probabilities
	default.data$frequency <- default.data$frequency/100
}

alpha <- 0.5
transprob <- exp(alpha*log(default.data$frequency))

# nonlinear regression
b0.seq <- seq(-8,1,1.0)
b1.seq <- seq( 0,3,0.5)
m0 <- length(b0.seq)
m1 <- length(b1.seq)

# find a good starting value by calculating some sums of squares
# rows are observations, columns are candidate b0/b1 values
ss.b0.b1 <- colSums((matrix(rep(transprob, m0*m1), 
		nrow=nrow(default.data), ncol=m0*m1) - 
			exp(alpha*cbind(1,default.data$rating) %*% 
				rbind(rep(b0.seq,m1),rep(b1.seq,each=m0))) )^2)
# if you want to see them all
cbind(rep(b0.seq,m1), rep(b1.seq,each=m0), ss.b0.b1 )
# easier to find the minimum
cbind(rep(b0.seq,m1), rep(b1.seq,each=m0))[ss.b0.b1==min(ss.b0.b1),]

# from the above seems like (-5,-5) is a decent place to start
fit.nls.tbs <- nls( transprob ~ exp(alpha*(b0 + b1*default.data$rating)), 
	start=list(b0=-5.0,b1=0.0))

fit.nls.tbs
summary(fit.nls.tbs)

# various statistics from proc univariate
mean(10*resid(fit.nls.tbs))
stdev(10*resid(fit.nls.tbs))
skewness(10*resid(fit.nls.tbs))
sum(10*resid(fit.nls.tbs)^2)
stdev(10*resid(fit.nls.tbs))/mean(10*resid(fit.nls.tbs))
sum(10*resid(fit.nls.tbs))
var(10*resid(fit.nls.tbs))
kurtosis(10*resid(fit.nls.tbs))
var(10*resid(fit.nls.tbs),SumSq=T)
stdev(10*resid(fit.nls.tbs))/sqrt(length(resid(fit.nls.tbs)))

shapiro.test(10*resid(fit.nls.tbs))
ks.gof(10*resid(fit.nls.tbs))




