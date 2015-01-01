############################################################
#                                                          #
# logLik.lm.q                                              #
#                                                          #
############################################################

# this a replacement for the S-Plus function logLik.lm
# the only change is the addition of "na.rm=T" to sum
# this allows functions like AIC and BIC to work on linear 
# models with missing data
#
# cgg
#
"logLik.lm" <- 
## log-likelihood for lm objects
function(object, REML = FALSE)
{
	res <- resid(object)
	p <- object$rank
	N <- length(res)
	if(is.null(w <- object$weights)) {
		w <- rep(1, N)
	}
	else {
		excl <- w == 0
		# eliminating zero weights
		if(any(excl)) {
			res <- res[!excl]
			N <- length(res)
			w <- w[!excl]
		}
	}
	N <- N - p * REML
	if(is.null(object$qr))
		object <- update(object, qr = T)
	# changed this line here---added na.rm=T
	val <- (sum(logb(w)) - N * (logb(2 * pi) + 1 - logb(N) + logb(sum(w * res^2, na.rm = T))))/2 - REML *
		sum(logb(abs(diag(object$R)[1:p])))
	attr(val, "nall") <- N + REML * p
	attr(val, "df") <- p + 1
	attr(val, "nobs") <- N
	oldClass(val) <- "logLik"
	val
}

