############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-10.q                                            #
#                                                          #
############################################################

# run figure12-9.q

# at each time t, compute the variance of the first t observations
# one way to do this: compute var(a1[(1:t)]) for each t
# that method is very very slow
# instead we'll use an updating formula
# can show (using some algebra)
#
# s2[n+1] = ((x[n+1] - mu[n])^2)/(n+1) + ((n-1)/n)*s2[n]
# 
# where s2[k] is the variance of the first k observations,
# mu[k] is the mean of the first k observations, and x[k]
# is the kth observation
#
# the slow part is computing the running means, but we already did that
# in figure12-9.q
#

v1 <- v2 <- v3 <- rep(NA, length(a1))
# variance of a constant is 0, not missing as var will tell ya
v1[1] <- v2[1] <- v3[1] <- 0 

for ( i in 2:length(a1) ) v1[i] <- ((i-2)/(i-1)) * v1[i-1] + ((a1[i] - a1.mean[i-1])^2)/i
for ( i in 2:length(a2) ) v2[i] <- ((i-2)/(i-1)) * v2[i-1] + ((a2[i] - a2.mean[i-1])^2)/i
for ( i in 2:length(a3) ) v3[i] <- ((i-2)/(i-1)) * v3[i-1] + ((a3[i] - a3.mean[i-1])^2)/i
	

par(mfrow=c(3,1),las=1)

plot( v1, type="l", main="alpha_1 = 0.9")
plot( v2, type="l", main="alpha_1 = 1.0")
plot( v3, type="l", main="alpha_1 = 1.8")

par(mfrow=c(1,1))
