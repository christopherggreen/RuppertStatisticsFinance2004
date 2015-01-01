############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# summary.ar.q                                             #
#                                                          #
############################################################

# this function produces SAS-like output for AR models fit 
# using ar()

"summary.ar" <- function(ar.model.fit) {

	# some formatting functions
	f <- function(x) format(x, sci=c(-8,8), justify="right"  , nsmall=6)
	g <- function(x) format(x, sci=c(-8,8), justify="decimal", nsmall=6)
	h <- function(x) format(round(x,6), sci=c(-8,8), justify="decimal", nsmall=6)
	
	dataset <- get(ar.model.fit$series)
	# number of observations
	n.obs <- if ( is.array(dataset) ) 
		dim(dataset)[1] else length(dataset)
	
	# ar coefficients
	ar.coefs.yw <- ar.model.fit$ar[,,1, drop=F]
	n.coefs     <- dim(ar.coefs.yw)[1]
	
	# intercept term ("mu") (ar removes the mean)
	intercept.yw    <- mean(dataset,na.rm=T)  	
	se.intercept.yw <- stdev(dataset,na.rm=T)/sqrt(n.obs)
	tstat.intercept.yw <- intercept.yw / se.intercept.yw
	
	dataset.demeaned <- dataset - intercept.yw
	
	# compute all the stuff SAS does

	# sum of squared errors (SSE)
	sse.yw  <- sum((ar.model.fit$resid)^2, na.rm=T)
	
	# sum of squares total (SST)
	sst.yw  <- sum((dataset.demeaned)^2,na.rm=T)
		
	# error degrees of freedom
	dfe.yw  <- ar.model.fit$n.used - n.coefs - 1
	
	# mean squared error
	mse.yw  <- sse.yw / dfe.yw
	
	# root mse
	rmse.yw <- sqrt(mse.yw)
	
	# akaike information criterion
	# and schwarz bayesian information criterion
	aic.yw  <- n.obs * log(mse.yw) 
	sbc.yw  <- aic.yw + log(n.obs)*n.coefs
	aic.yw  <- aic.yw + 2*n.coefs

	# regression and total R-square
	regr2.yw <- 0
	totr2.yw <- 1 - (sse.yw / sst.yw )
	
	# std. errors of ar estimates
	# yule-walker gamma's
	gammas <- rep(NA, 1+n.coefs)
	gammas[1] <- sst.yw
	for ( i in seq(n.coefs) )
		gammas[i+1] <- sum( dataset.demeaned[seq(n.obs-i)] * dataset.demeaned[-seq(i)], na.rm=T ) 
	gammas <- gammas/n.obs	
	gamma.mat <- 	outer(seq(n.coefs),seq(n.coefs), function(x,y,g) g[1+abs(x-y)], g=gammas)
		
	se.ar.yw <- rmse.yw * sqrt(diag(solve(gamma.mat))) / sqrt(n.obs)
	
	###################################################	
	# nice tabular output		
	###################################################
	
	cat("\n",f(c(
		"Variable","DF","Estimates","Std. Error","t-value","Pr > |t|","\n",
		"Intercept","1",h(intercept.yw),h(se.intercept.yw),h(tstat.intercept.yw),
			h(round(2*pt(-abs(tstat.intercept.yw), df=1))), "\n"
		)),sep="")	
		
	cat("Estimates of Autocorrelations\n")
	cat("\n",f(c(
		" Lag"," Covariance"," Correlation","\n",
		as.vector(
			rbind(
				as.character(seq(length=n.coefs+1)-1),
				h( (mse.yw/(1 - sum(ar.coefs.yw^2))) * c(1.0, ar.coefs.yw) ),
				h(c(1.0, ar.coefs.yw)),
				rep("\n", n.coefs+1)
			)
		),
		"\n",
		)), sep="")
	cat("\n")
	cat(" Yule-Walker Estimates \n\n")
	cat("Estimates of Autoregressive Parameters\n")
	cat("\n",g(c(
		" Lag"," Coefficient"," Std. Error"," t-value","\n",
		as.vector(	
			rbind( 
				as.character(seq(length=n.coefs)), 
				h(ar.coefs.yw),
				h(se.ar.yw),
				h(ar.coefs.yw/se.ar.yw),
				rep("\n",n.coefs)
			)
		),
		"\n"
		)),sep="")
	cat("\n")
	
	cat("\n",f(c(
		"SSE",h(sse.yw),"DFE",h(dfe.yw),"\n",
		"MSE",h(mse.yw),"Root MSE",h(rmse.yw),"\n",
		"SBC",h(sbc.yw),"AIC", h(aic.yw),"\n",
		"Regress R-Square",h(regr2.yw),"Total R-Square",h(totr2.yw),"\n",
		"Durbin-Watson",
			g(durbinWatson(ar.model.fit$resid)$statistic),"\n"
		)),sep="")
	
	cat("\n",f(c(
		"Variable","DF","Estimates","Std. Error","t-value","Pr > |t|","\n",
		"Intercept","1",h((1-sum(ar.coefs.yw))*intercept.yw),h(se.intercept.yw),h(tstat.intercept.yw),
			h(round(2*pt(-abs(tstat.intercept.yw), df=1))), "\n"
		)),sep="")	
		
	cat("\n")	
	NULL
}

