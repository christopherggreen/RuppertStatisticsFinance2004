############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-02.q                                             #
#                                                          #
############################################################
 
# replicates Figure 2.2, page 17
x <- 0:10
# dbinom gives the density function for the binomial distribution
y <- dbinom(x, 10, .6)
barplot(y, xlab="x", ylab="P(X=x)", names=as.character(x), las=1)
box()
