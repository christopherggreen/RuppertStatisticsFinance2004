############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page190.q                                                #
#                                                          #
############################################################

# load bond prices data
bondprices <- read.table("../common/data/bondprices.txt",header=F,skip=4)
names(bondprices) <- c("maturity","price")

n <- nrow(bondprices)

# sum of squares for r values listed in SAS code
rr <- seq(0.02,0.09,0.005)
nrr <- length(rr)

cbind(rr,rowSums((matrix(rep(bondprices$price,nrr), nrow=nrr, byrow=T) - 
	1000*exp(matrix(-rr,ncol=1) %*% matrix(bondprices$maturity,nrow=1)))^2))

fit <- nls( price ~ 1000*exp(-r*maturity), data=bondprices, start=list(r=0.025) )


ssr  <- sum((predict(fit)-mean(bondprices$price))^2)
sse  <- sum(resid(fit)^2)
sst  <- sum((bondprices$price-mean(bondprices$price))^2)
ssut <- sum(bondprices$price^2)

fit.anova <- data.frame(list(
	"Source"=c("Regression","Residual","Uncorrected Total","Corrected Total"),
	"DF"=c(1,n-1,n,n-1),
	"Sum Sq"=c(ssr,sse,ssut,sst),
	"Mean Sq"=c(ssr, sse/(n-1), NA, NA),
	"F Value"=rep(NA,4),
	"Pr(F)"=rep(NA,4)),
	check.names=F)
fit.anova[1,"F Value"] <- fit.anova[2,"Mean Sq"]/fit.anova[1,"Mean Sq"]
fit.anova[1,"Pr(F)"] <- 1 - pf(fit.anova[1,"F Value"], 1, n-1)

	
summary(fit)
