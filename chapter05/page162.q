############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page162.q                                                #
#                                                          #
############################################################

############################################################
#                                                          #
# S-Plus does not have any built-in facilities for doing   #
# quadratic programming. The add-on module S+NuOPT has a   #
# function, solveQP, that is functionally equivalent to    #
# MATLAB's quadprog. S+NuOPT is rather expensive, however. #
#                                                          #
# In R the library quadprog (available on CRAN) has a      #
# function solve.QP that will work. (You can't use that    #
# library as is under S-Plus, though. The source for       #
# quadprog is available on StatLib, but you need a Fortran #
# compiler to build it for S-Plus. Good luck with that...) #
#                                                          #
############################################################

tryval <- try(module(nuopt)) # for solveQP
if ( is(tryval, "Error") )
	warning("NuOPT is not installed. solveQP is not available.")

bmu <- matrix(c(0.08,0.03,0.05), ncol=1)
bOmega <- matrix(c(0.3,0.02,0.01,0.02,0.15,0.03,0.01,0.03,0.18), nrow=3, ncol=3)
Aeq <- rbind(1,t(bmu))
ngrid <- 50
muP <- seq(0.03, 0.08, length=ngrid)
# note: rf is a function in S-Plus
rf.rate <- 0.04

sigmaP <- muP # set up storage
omegaP <- matrix(0, nrow=3, ncol=ngrid)

for ( i in seq(1,ngrid) ) {
	# solveQP is the nearest equivalent to quadprog in S-Plus
	# requires the NuOPT module
	# syntax in MATLAB's notation:
	# solveQP(H,f,rbind(A,Aeq),
	#          c(rep(0,nrow(A),beq),
	#          c(b,beq), LB, UB)
	#
	omegaP[,i] <- solveQP(
		objQ=bOmega, 
		objL=rep(0,nrow(bmu)), 
		A=Aeq, 
		cLO=c(1, muP[i]), 
		cUP=c(1, muP[i]), 
		bLO=rep(0,nrow(bOmega)),
		bUP=rep(Inf,nrow(bOmega)),
		trace=F)$variables$x$current
	#print(omegaP[,i])
	sigmaP[i] <- sqrt(t(omegaP[,i]) %*% bOmega %*% omegaP[,i])
}

imin <- which(sigmaP==min(sigmaP))
Ieff <- which(muP >= muP[imin])
sharperatio <- (muP - rf.rate) / sigmaP
Itangency <- which(sharperatio==max(sharperatio))

