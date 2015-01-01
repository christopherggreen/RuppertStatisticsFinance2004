############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page247.q                                                #
#                                                          #
############################################################

# the data set used by Ruppert does not match the data set on
# his website with that name
# so, we took the data set we had---daily price data---computed
# log returns, then coarsened the series to monthly data
#
# our numbers will differ from Ruppert's, but hopefully this
# illustrates the procedure. 

capm <- read.table("../common/data/capm.txt",header=F,skip=10,row.names=NULL,as.is=T)
names(capm) <- c("tbill","microsoft","sp500","ge","ford","day","month","year")

capm$year[capm$year > 90] <- 1900 + capm$year[capm$year > 90]
capm$year[capm$year < 10] <- 2000 + capm$year[capm$year < 10]

capm$date <- timeDate(paste(capm$month,capm$day,capm$year), in.format="%m %d %Y", 
	format="%02m/%02d/%4Y")
capm <- capm[timeDate("03/01/1996") <= capm$date & capm$date <= timeDate("02/28/2001"),]

# compute log returns
capm$ford.logret  <- c(NA,diff(log(capm$ford )))
capm$sp500.logret <- c(NA,diff(log(capm$sp500)))
capm$ge.logret  <- c(NA,diff(log(capm$ge )))
capm$microsoft.logret <- c(NA,diff(log(capm$microsoft)))
capm.logret <- timeSeries(
	capm[,c("microsoft.logret","sp500.logret","ge.logret","ford.logret")],positions=capm$date)

# coarsen from daily to monthly
capm.logret.monthly <- data.frame(seriesData(aggregateSeries(capm.logret, 
		by="months", FUN=function(x)sum(x,na.rm=T), offset=25)))
# compute excess returns
# first accumulate daily t-bill returns
tbill.return.monthly <- drop(seriesData(aggregateSeries(timeSeries(capm$tbill/(100*253),positions=capm$date),
	 by="months", FUN=function(x) prod(1+x)-1, offset=25)))
		
capm.logret.monthly$ford.logret  <- capm.logret.monthly$ford.logret - tbill.return.monthly
capm.logret.monthly$sp500.logret <- capm.logret.monthly$sp500.logret - tbill.return.monthly
capm.logret.monthly$ge.logret    <- capm.logret.monthly$ge.logret - tbill.return.monthly
capm.logret.monthly$microsoft.logret <- capm.logret.monthly$microsoft.logret - tbill.return.monthly


capm.logret.monthly$sp500.logret2 <- capm.logret.monthly$sp500.logret
capm.logret.monthly$sp500.logret2[capm.logret.monthly$sp500.logret2 <= 0] <- 0

model.1 <- lm( ford.logret ~ sp500.logret, data=capm.logret.monthly, na.action=na.omit)
model.2 <- lm( ford.logret ~ sp500.logret + sp500.logret2, data=capm.logret.monthly, na.action=na.omit)
model.3 <- lm( ford.logret ~ sp500.logret - 1, data=capm.logret.monthly, na.action=na.omit)

anova(model.1)
summary(model.1)
AIC(model.1)
summary.proc.reg(model.1)

anova(model.2)
summary(model.2)
AIC(model.2)
summary.proc.reg(model.2)

anova(model.3)
summary(model.3)
AIC(model.3)
summary.proc.reg(model.3)



