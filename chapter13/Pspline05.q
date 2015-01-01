############################################################
#                                                          #
# Pspline05.q                                              #
#                                                          #
############################################################

# direct translation of Ruppert's MATLAB code

Pspline05 <- 

#	Fits a P-spline to univariate x's with Demmler-Reinsch
#	algorithm.
#
#	Copied from PsolineDR02 - like that program but optional input
#		enters as a structure.
#
#	Also allows a quadratic integral penalty on the 2nd derivative
#
#
#	USAGE:  fit = Pspline05(x,y,param)
#
#
#
#		INPUT - REQUIRED
#	x = independent variable (univariate)
#	y = response (same length as x)
#
#		INPUT - OPTIONAL (put in a structure that is the third
#			argument in)
#	degree = degree of the spline (default is 2) (changed to 3 if
#			smooth.spline.penalty = 1)
#	nknots = number of knots (default min of floor(.3*n) and 20)
#	extrapen: if 1 then the x^degree term is penalized if 0 then not
#			(default is 0)
#	penwt = trial values of the penalty weight (one is chosen by 
#		minimizing gcv (default is logspace(-12,12,100))
#	boundstab = parameter passed to quantileknots (see that program)
#	knots (default is to generate nknots using the program
#		quantileknots)
#	istd = 1 if x and y are to be standardized before computation
#		(default = 1)
#	smooth.spline.penalty = if 1 then penalty is on the
#		integral from min(x) to max(x) of the square
#		of the second derivative
#   igcv = if 1 then uses gcv.  Otherwise uses REML
#	
#		OUTPUT
#	Returns a structure "fit" with the following components
#
#	CALLS: powerbasis01, quantileknots01
#
#	Last edit: 5/5/04
#
#
# cgg: elements of param are now optional arguments with defaults
#
function(x,y, 
	knots,
	gcvfact=T, 
	istd=T, 
	penwt=10^seq(-6,5,length=200),
	nknots=min(c(floor(.3*n),20)),
	degree=2,
	smooth.spline.penalty=F,
	igcv=T
	)
{
	x <- as.vector(x)
	n <- length(x)

	if ( missing(knots) ) {
		knots <- quantileknots01(x,nknots)
		nknots <- length(knots)
	}

	originalx <- x 
	stdx      <- stdev(x) 
	meanx     <- mean(x) 

	if (istd) {
		x <- (x - meanx) / stdx 
		meany <- mean(y) 
		y <- y - meany 
		knots <- (knots - meanx) / stdx 
	} 

	if (smooth.spline.penalty) 
		degree <- 3 

	xm <- powerbasis01(x,degree,knots) 
	xx <- t(xm) %*% xm 

	xm1 <- xm[,1:(1+degree)] 
	xm2 <- xm[,(2+degree):ncol(xm)] 
	xmxm <- xx 

	xm1xm1 <- t(xm1) %*% xm1 
	xm1xm2 <- t(xm1) %*% xm2 
	xm2xm2 <- t(xm2) %*% xm2 

	eyye <- diag(rep(1,nknots))

	#id <- rep(c(0,1),c(degree+1, nknots))
	#D  <- diag(id)
	 
	D  <- diag(rep(c(0,1),c(degree+1, nknots)))

	if (smooth.spline.penalty) {
		D2     <- D 
		maxx   <- max(x) 
		knots2 <- c(min(x), knots)
		
		D[,1:2] <- D[1:2,]  <- 0
		D[3,3]   <- 4
		D[-(1:3),3] <- D[3,-(1:3)] <- 6*((maxx - knots2)^2)

		for (i in 4:(4+nknots) ) {
			for (i2 in 4:(4+ nknots) )  {
				knotmax <- knots2[ max(c(i-3,i2-3)) ] 
				knotmin <- knots2[ min(c(i-3,i2-3)) ] 
				D[i,i2] <- 36*( ((maxx-knotmax)^3)/3 + (knotmax - knotmin) * ((maxx-knotmax)^2)/2) 
			} 	
		} 	
	}  	

	R  <- chol(xx + 1e-9*(if (smooth.spline.penalty) D2 else D))
	B  <- solve(t(R))
	UC <- eigen(B %*% (D %*% t(B)))
	U  <- UC$vectors
	C  <- UC$values
	Z  <- xm %*% (t(B) %*% U)

	Zy <- t(Z) %*% y 
	ZZ <- t(Z) %*% Z 

	m     <- length(penwt)  
	beta  <- matrix(0, nrow=ncol(xm), ncol=m) 
	dfres <- trsdsd <- dffit <- gcv <- lrl <- asr  <- matrix(NA, nrow=m, ncol=1)
	ssy   <- t(y) %*% y 
	alpha <- matrix(NA, nrow=nrow(Zy), ncol=m)
	
	for (i in 1:m) {
		oneld     <- matrix(1 / (1 + penwt[i]*C),ncol=1)
		dffit[i]  <- sum(oneld) 
		alpha[,i] <- Zy * oneld 
		asr[i]    <- (ssy - 2*(t(Zy) %*% alpha[,i]) + (t(alpha[,i]) %*% (ZZ %*% alpha[,i])))/n 
		trsdsd[i] <- sum(oneld^2) 
		dfres[i]  <- n - 2*dffit[i] + trsdsd[i] 
		gcv[i]    <- asr[i] / ((1 - gcvfact*dffit[i]/n)^2)
		sigma2    <- asr[i] / ((1 - dffit[i]/n))
		# for the early steps, the (penwt[i]*eyye + xm2xm2) matrix
		# is ill-conditioned, so solve will fail.
		# use try to get around this
		lrl[i] <- -.5 * ( (n-degree-1)*log(sigma2) + (n-degree-1) +
			log(det(penwt[i]*eyye + xm2xm2 )) - nknots*log(penwt[i]) )			
		gg <- try( solve(penwt[i]*eyye + xm2xm2)	)
		if ( !is(gg,"Error") )
			lrl[i] <- lrl[i] + log(det(xm1xm1 - xm1xm2 %*% (gg %*% t(xm1xm2))))
		
	} 

	imin <- min(which(if (igcv) gcv==min(gcv) else lrl==max(lrl)))
	a    <- penwt[imin] 

	alpha     <- alpha[,imin] 
	beta      <- as.vector(t(B) %*% (U %*% alpha))
	dbeta     <- length(beta) 
	yhat      <- Z %*% alpha 
	res       <- y - yhat 
	sigma2hat <- n * (asr / dfres)
	sigmahat  <- sqrt(sigma2hat) 
	ones.dbeta <- matrix(1, nrow=dbeta, ncol=1)
	
	oneld        <- 1 / (1 + penwt[imin]*C)
	postvaralpha <- sigma2hat[imin] * diag(oneld)
	varalpha     <- sigma2hat[imin] * diag(oneld^2)

	postvaryhat  <- (Z * (Z %*% postvaralpha)) %*% ones.dbeta
	postvarbeta  <- t(B) %*% (U %*% postvaralpha %*% t(U)) %*% B 
	varbeta      <- t(B) %*% (U %*% varalpha %*% t(U)) %*% B 
	varyhat      <- (Z * (Z %*% varalpha)) %*% ones.dbeta 

	if ( n < 5001 ) {
		hi <- (Z * (Z %*% diag(oneld))) %*% matrix(1,nrow=ncol(Z),ncol=1)
		cookD <- (res^2) * (hi / ( dffit[imin]*(1-hi) ) )
		studres <- res * (sqrt(1-hi)) / sigmahat[imin] 
	}

	Z <- NULL 

	yhatder <- postvaryhatder <- NA 
	if (degree > 0) {
		xmder <- xm 
		xmder[,1] <- 0
		
		for (i in 2:(degree+1))
			xmder[,i] <- (i-1)*(abs(xm[,i])^((i-2)/(i-1)))*(sign(x)^(i-2))
		
		for (i in (degree+2):(degree+1+nknots))
			xmder[,i] <- ifelse(xm[,i]>0, degree*(abs(xm[,i])^((degree-1)/degree)), 0)
		
		yhatder        <- xmder %*% beta 
		postvaryhatder <- ((xmder %*% postvarbeta)*xmder) %*% ones.dbeta 
		varyhatder     <- ((xmder %*% varbeta)*xmder) %*% ones.dbeta
	} 

	if (istd )
		yhat <- yhat + meany 

	ulimit    <- yhat + 2*sqrt(postvaryhat) 
	llimit    <- yhat - 2*sqrt(postvaryhat) 
	ulimitder <- yhatder + 2*sqrt(postvaryhatder) 
	llimitder <- yhatder - 2*sqrt(postvaryhatder) 

	xgrid  <- seq(min(x),max(x),length=1000) 
	xgridm <- powerbasis01(xgrid,degree,knots) 
	#sizexgridm <- length(xgridm) 

	mhat <- xgridm*beta 
	if (istd ) {
		xgrid          <- meanx + stdx*xgrid 
		mhat           <- meany + mhat 
		knots          <- meanx + stdx*knots 
		yhatder        <- yhatder/stdx 
		ulimitder      <- ulimitder/stdx 
		llimitder      <- llimitder/stdx 
		varyhatder     <- varyhatder/(stdx^2) 
		postvaryhatder <- postvaryhatder/(stdx^2) 

		ideg <- 1:(degree+1)
		beta[ ideg] <- beta[ideg]/(stdx^(ideg-1))
		beta[-ideg] <- beta[-ideg]/stdx^degree
	} 

	i <- which(!duplicated(originalx))

	ulimit.xgrid <- approx(spline(originalx[i],ulimit[i]),xout=xgrid)$y
	llimit.xgrid <- approx(spline(originalx[i],llimit[i]),xout=xgrid)$y 

	ulimitder.xgrid <- approx(spline(originalx[i],ulimitder[i]),xout=xgrid)$y
	llimitder.xgrid <- approx(spline(originalx[i],llimitder[i]),xout=xgrid)$y 

	mhatder <- approx(spline(originalx[i],yhatder[i]),xout=xgrid)$y 

	
	fit <- list("yhat"=yhat, "beta"=beta, "gcv"=gcv,
			"imin"=imin, "dffit"=dffit, "knots"=knots,
			"postvarbeta"=postvarbeta, "postvaryhat"=postvaryhat, "xm"=xm,
			"xx"=xx, "a"=a, "penwt"=penwt, "sigma2hat"=sigma2hat, "dfres"=dfres,
			"yhatder"=yhatder, "postvaryhatder"=postvaryhatder, "ulimit"=ulimit,
			"llimit"=llimit, "ulimitder"=ulimitder, "llimitder"=llimitder,
			"asr"=asr, "xgrid"=xgrid, "mhat"=mhat, "x"=meanx+stdx*x,
			"degree"=degree, "varbeta"=varbeta, "varyhat"=varyhat, 
			"varyhatder"=varyhatder,
			"lrl"=lrl,
		)

	if (n < 5001)
		fit <- c(fit, 
			list( "istd"=istd,
				"ulimit.xgrid"=ulimit.xgrid, "llimit.xgrid"=llimit.xgrid, "ulimitder.xgrid"=ulimitder.xgrid,
				"llimitder.xgrid"=llimitder.xgrid, "hi"=hi, "res"=res, "cookD"=cookD, "studres"=studres, 
				"xmder"=xmder,"mhatder"=mhatder
			))

	fit
}
