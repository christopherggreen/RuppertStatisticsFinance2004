############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure10-9.q                                             #
#                                                          #
############################################################

# run figure10-5.q first

par(mfrow=c(3,1),xaxs="i",yaxs="i",las=1)
plot( sigmaP.true, muP.true, lwd=3, type="l",
	xlab="standard deviation of return (sigma_p)",
	ylab="expected return (mu_p)", xlim=c(0.03,0.06), ylim=c(0.007,0.020), lwd=3 )
omegaP.012.true <- bg.true + 0.012 * bh.true
sigmaP.012.true <- sqrt(t(omegaP.012.true) %*% bOmega.true %*% omegaP.012.true)
points( sigmaP.012.true, 0.012, pch=16, cex=3)
key(corner=c(0,1), border=T,	points=list(pch=c(45,16,46),cex=c(1,1.5,1)),
	text=list(c("achieved","optimal","eff. frontier")) )

# resample versions
nresample <- 400
muP.012.resample    <- rep(NA,nresample)
sigmaP.012.resample <- rep(NA,nresample)

# note: this is not the most efficient way to do this!
n <- nrow(countries.logreturns)
for ( i in 1:nresample ) {
	# resampling with 3 times the sample size
	resampled.returns <- countries.logreturns[sample(1:n, size=3*n, replace=T),]
		
	bmu.resample    <- as.matrix(colMeans(resampled.returns))
	bOmega.resample <- var(resampled.returns)
	ibOmega.resample <- solve(bOmega.resample)
	A.resample <- as.numeric(t(bone) %*% ibOmega.resample %*% bmu.resample)
	B.resample <- as.numeric(t(bmu.resample)  %*% ibOmega.resample %*% bmu.resample)
	C.resample <- as.numeric(t(bone) %*% ibOmega.resample %*% bone)
	D.resample <- B.resample * C.resample - A.resample^2

	bg.resample <- (B.resample * ibOmega.resample %*% bone - A.resample * ibOmega.resample %*% bmu.resample )/D.resample
	bh.resample <- (C.resample * ibOmega.resample %*% bmu.resample  - A.resample * ibOmega.resample %*% bone)/D.resample

	#gg.resample <- t(bg.resample) %*% bOmega.resample %*% bg.resample
	#hh.resample <- t(bh.resample) %*% bOmega.resample %*% bh.resample
	#gh.resample <- t(bg.resample) %*% bOmega.resample %*% bh.resample

	omegaP <- bg.resample + 0.012*bh.resample
	sigmaP.012.resample[i] <- sqrt(t(omegaP) %*% bOmega.true %*% omegaP)
	muP.012.resample[i]    <- as.numeric(t(omegaP) %*% bmu.true)
}
points( sigmaP.012.resample, muP.012.resample, pch="." )

hist( sigmaP.012.resample/sigmaP.012.true, xlab="sigma_p/sigma_{p,opt}" )
hist( muP.012.resample/0.012, xlab="mu_p/0.012" )

par(mfrow=c(1,1))


