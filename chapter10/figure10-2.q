############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure10-2.q                                             #
#                                                          #
############################################################

# load data
countries <- read.table("../common/data/countries.dat", skip=15)
countries.names <- c("hong kong","singapore","brazil",
	"argentina","uk","germany","canada","france","japan","us")
names(countries) <- c("month","day","year", countries.names)

# convert date to timeDate object for nice plots
countries$date <- timeDate(paste(countries$month,countries$day,
	1900+ifelse(countries$year < 6,100,0) + countries$year,sep="/"), 
	in.format=	"%m/%d/%Y")

# compute log returns
countries.logreturns <- matrix(NA, ncol=length(countries.names), nrow=nrow(countries)-1)
dimnames(countries.logreturns) <- list(NULL, countries.names)
for ( country in countries.names	)
	countries.logreturns[,country] <- diff(log(countries[[country]]))
		
# 10.2
par(mfrow=c(5,2),xaxs="i",yaxs="i",las=1)
for ( country in 	countries.names ) {
	plot(countries.logreturns[,country], type="l", ylab="return", 
	xlab="", ylim=c(-1,1), main=country, cex=0.55, lwd=1)
}
par(mfrow=c(1,1))



	
