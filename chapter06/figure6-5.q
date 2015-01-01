############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-5.q                                              #
#                                                          #
############################################################

# run figure6-4.q first
par(mfrow=c(2,1), las=1, lwd=3)
plot(ls.diag(fit1)$cooks,xlab="case number",ylab="Cook's D",pch="+")
plot(ls.diag(fit2)$cooks,xlab="case number",ylab="Cook's D",pch="+")
par(mfrow=c(1,1))
