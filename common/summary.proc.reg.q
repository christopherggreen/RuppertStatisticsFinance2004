############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# summary.proc.reg.q                                       #
#                                                          #
############################################################

# produces SAS-like (a la proc reg) output from linear model
# objects in S-Plus

summary.proc.reg <- function(lmobj) {
	
	# some formatting functions
	f <- function(x) format(x, sci=c(-8,8), justify="right"  , nsmall=6)
	g <- function(x) format(x, sci=c(-8,8), justify="decimal", nsmall=6)
	h <- function(x) format(round(x,6), sci=c(-8,8), justify="decimal", nsmall=6)

	if ( is.null(lmobj$call$data) )
		dependent.series <- eval(as.list(formula(lmobj))[[2]])
	else 
		dependent.series <- eval(as.list(formula(lmobj))[[2]], local=eval(lmobj$call$data))
	# if there were missing values in one of the terms,
	# reduce dependent.series to reflect only values used in the
	# computations
	if ( !is.null(lmobj$na.action) )
		dependent.series <- dependent.series[-lmobj$na.action]
		

	# modify output of anova to match SAS's format
	zzz <- anova(lmobj)
	model.rows <- grep("Residuals",row.names(zzz))
	if ( length(model.rows) == 0 ) 
		stop("ANOVA object lacks \"Residuals\" row?")
	attr(zzz,"heading")[2] <- gsub("^Response","Dependent Variable",attr(zzz,"heading")[2])
	attr(zzz,"heading")[3] <- ""
	zzz[1,c("Df","Sum of Sq")] <- colSums(zzz[-model.rows,c("Df","Sum of Sq"),drop=F])
	zzz <- zzz[c(1,model.rows),,drop=F]
	zzz[1,"Mean Sq"] <- zzz[1,"Mean Sq"]/zzz[1,"Df"]
	row.names(zzz)[1] <- "Model"
	row.names(zzz)[2] <- "Error"
	yyy <- data.frame(as.list(colSums(zzz[,c("Df","Sum of Sq"),drop=F])),
		as.list(c(NA,NA,NA)))
	names(yyy) <- names(zzz)
	row.names(yyy) <- "Corrected Total"
	zzz <- rbind(zzz, yyy)
	zzz[1,"F Value"] <- zzz[1,"Mean Sq"]/zzz[2,"Mean Sq"]
   # print out modified anova table
	print(zzz)
	cat("\n")
	
	# now create a table showing root mse, etc.
	xxx <- data.frame(list(c("Root MSE","Dependent Mean","Coeff Var"),
		rep(1.,3), c("R-Square","Adj R-Sq",""), rep(1.,3)), stringsAsFactors=F)
	names(xxx) <- c(" ","  ","   ","    ")
	row.names(xxx) <- c(" ","  ","   ")
	n <- zzz[3,"Df"] # actually n-1
	p <- zzz[1,"Df"]
	zzz[1,"Pr(F)"] <- if (zzz[1,"Pr(F)"] < 0.0001 ) "<.0001" else zzz[1,"Pr(F)"]
	# root mse
	xxx[1,2] <- h(sqrt(zzz[2,"Mean Sq"]))
	# dependent mean
	xxx[2,2] <- h(mean(dependent.series))
	# coefficient variation (SAS defines this as 100 * (RMSE/y.bar))
	xxx[3,2] <- h(100*sqrt(zzz[2,"Mean Sq"])/mean(dependent.series))
	# r-squared
	xxx[1,4] <- h(summary(lmobj)$r.squared)
	# adjusted r-squared
	xxx[2,4] <- h(1 - ((n/(n-p))*(1-summary(lmobj)$r.squared)))
	xxx[3,4] <- ""
	# finished, print table
	print(xxx)
	cat("\n")
	
	# construct parameter estimates table
	cat("\t\tParameter Estimates\n")
	cat("\n")
	cat("                 Parameter\tStandard\n")
	cat("Variables  \tDF\t Estimate\t   Error\tt Value\tPr > |t|\n")
	cat("\n")
	coef.mat  <- h(summary(lmobj)$coefficients)
	var.names <- g(dimnames(coef.mat)[[1]])
	haveIntercept <- ( length(grep("Intercept",var.names)) > 0 )
	n.coef <- dim(coef.mat)[1]
	deg.free  <- rep(1, n.coef)
	other.mat <- array(NA, c(n.coef,3))
	# Type I sum of squares
	other.mat[,1] <- summary.aov(lmobj,intercept=haveIntercept,ssType=1)[-(n.coef+1),"Sum of Sq"]
	# Type II sum of squares
	if ( length(var.names) > 1 )
		other.mat[,2] <- drop1(lmobj)[,"Sum of Sq"]
	# otherwise leave as missing
	
	# variance inflation factors
	if ( haveIntercept ) {
		other.mat[1,3] <- 0 # intercept
		other.mat[-1,3] <- vif(lmobj)	
	} else {
		if ( length(var.names) > 1 )
			other.mat[,3] <- vif(lmobj)
		# otherwise missing
	}
	other.mat <- h(other.mat)
	dimnames(other.mat)<-list(NULL,c("Type I SS","Type II SS","VIF"))
	for ( i in seq(along=var.names) ) {
		cat(var.names[i],"\t",deg.free[i],"\t",paste(coef.mat[i,],collapse="\t"),"\n")
	}	
	cat("\n")
	cat("Variables  \tDF\tType I SS\t Type II SS\t Var. Inf\n")
	cat("\n")
	for ( i in seq(along=var.names) ) {
		cat(var.names[i],"\t",deg.free[i],"\t",paste(other.mat[i,],collapse="\t"),"\n")
	}
	invisible(NULL)
}
