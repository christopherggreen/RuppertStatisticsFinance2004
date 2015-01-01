############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-9.q                                             #
#                                                          #
############################################################

# save the running means---we'll use them in figure12-10.q

a1.mean <- cumsum(a1)/seq(along=a1)
a2.mean <- cumsum(a2)/seq(along=a2)
a3.mean <- cumsum(a3)/seq(along=a3)

par(mfrow=c(3,1),las=1)

plot(a1.mean, type="l", main="alpha_1 = 0.9")
plot(a2.mean, type="l", main="alpha_1 = 1.0")
plot(a3.mean, type="l", main="alpha_1 = 1.8")

par(mfrow=c(1,1))
