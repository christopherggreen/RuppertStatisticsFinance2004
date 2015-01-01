############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-1.q                                             #
#                                                          #
############################################################

# load data
eurodata <- read.table("../common/data/euro_rates.dat",skip=8)
names(eurodata) <- c("month","day","year","eu01", "eu03","eu06")

# convert from percentages
eurodata[,c("eu01","eu03","eu06")] <- eurodata[,c("eu01","eu03","eu06")]/100

# build a timeSeries object for plotting
eurodata$date <- timeDate(paste(eurodata$month,eurodata$day,
	1900+eurodata$year+ifelse(eurodata$year < 10,100,0), sep="/"), in.format="%m/%d/%Y")
eurodata.ts <- timeSeries(eurodata[c("eu01","eu03","eu06")], positions=eurodata$date)


par(mfrow=c(2,1), las=1)
plot( eurodata.ts[,"eu01"], ylab="Euro rate", xlab="Year", reference.grid=F )
plot( diff(eurodata.ts[,"eu01"]), ylab="Weekly change", xlab="Year", reference.grid=F)
par(mfrow=c(1,1))
