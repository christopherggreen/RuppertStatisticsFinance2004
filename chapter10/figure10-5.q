############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure10-5.q                                             #
#                                                          #
############################################################

# matrix of 1's
bone        <- matrix(rep(1,ncol(countries.logreturns)), ncol=1)

# compute actual efficient frontier
# using code from Chapter 5
bmu.true    <- as.matrix(colMeans(countries.logreturns))
bOmega.true <- var(countries.logreturns)

ibOmega.true <- solve(bOmega.true)

A.true <- as.numeric(t(bone) %*% ibOmega.true %*% bmu.true)
B.true <- as.numeric(t(bmu.true)  %*% ibOmega.true %*% bmu.true)
C.true <- as.numeric(t(bone) %*% ibOmega.true %*% bone)
D.true <- B.true * C.true - A.true^2

bg.true <- (B.true * ibOmega.true %*% bone - A.true * ibOmega.true %*% bmu.true )/D.true
bh.true <- (C.true * ibOmega.true %*% bmu.true  - A.true * ibOmega.true %*% bone)/D.true

gg.true <- t(bg.true) %*% bOmega.true %*% bg.true
hh.true <- t(bh.true) %*% bOmega.true %*% bh.true
gh.true <- t(bg.true) %*% bOmega.true %*% bh.true

ngrid       <- 500
muP.true    <- seq(0,0.3,length=ngrid) #seq(min(bmu.true), max(bmu.true), length=ngrid) # muP grid
sigmaP.true <- rep(0,ngrid)                          # storage
for ( i in 1:ngrid ) {
        omegaP <- bg.true + muP.true[i]*bh.true
        sigmaP.true[i] <- sqrt(t(omegaP) %*% bOmega.true %*% omegaP)
}

# Create plot - efficient frontier is shown as a solid curve
# - the inefficient part of the locus is dashed

par(mfrow=c(3,2),xaxs="i",yaxs="i",las=1)
for (i in 1:6 ) {
	plot( sigmaP.true, muP.true, lwd=3, type="l",
		xlab="standard deviation of return (sigma_p)",
		ylab="expected return (mu_p)", xlim=c(0.03,0.06), ylim=c(0.007,0.020) )
	# sample with replacement from the rows
	resampled.returns <- countries.logreturns[sample(nrow(countries.logreturns), replace=T),]

	# compute actual efficient frontier
	bmu.resample    <- as.matrix(colMeans(resampled.returns))
	bOmega.resample <- var(resampled.returns)

	ibOmega.resample <- solve(bOmega.resample)

	A.resample <- as.numeric(t(bone) %*% ibOmega.resample %*% bmu.resample)
	B.resample <- as.numeric(t(bmu.resample)  %*% ibOmega.resample %*% bmu.resample)
	C.resample <- as.numeric(t(bone) %*% ibOmega.resample %*% bone)
	D.resample <- B.resample * C.resample - A.resample^2

	bg.resample <- (B.resample * ibOmega.resample %*% bone - A.resample * ibOmega.resample %*% bmu.resample )/D.resample
	bh.resample <- (C.resample * ibOmega.resample %*% bmu.resample  - A.resample * ibOmega.resample %*% bone)/D.resample

	gg.resample <- t(bg.resample) %*% bOmega.resample %*% bg.resample
	hh.resample <- t(bh.resample) %*% bOmega.resample %*% bh.resample
	gh.resample <- t(bg.resample) %*% bOmega.resample %*% bh.resample

	muP.resample    <- rep(0,ngrid)
	sigmaP.resample <- rep(0,ngrid)                          # storage
	for ( i in 1:ngrid ) {
			omegaP <- bg.resample + muP.true[i]*bh.resample
			sigmaP.resample[i] <- sqrt(t(omegaP) %*% bOmega.true %*% omegaP)
			muP.resample[i] <- as.numeric(t(omegaP) %*% bmu.true)
	}
	lines( sigmaP.resample, muP.resample, lwd=3, lty=3)		
}	
par(mfrow=c(1,1))
