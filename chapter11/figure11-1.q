############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure11-1.q                                             #
#                                                          #
############################################################

# load data
sp500 <- read.table("../common/data/capm.txt",skip=10)[,c(3,6:8)]
names(sp500) <- c("sp500","day","month","year")

# adjust years to 4 digits
sp500$year <- 1900 + sp500$year + ifelse(sp500$year < 10, 100, 0)

# create a timeSeries object for a nice plot
sp500.logret.ts <- timeSeries( diff(log(sp500$sp500)), 
	positions=timeDate(paste(sp500$month,sp500$day,sp500$year,sep="/"), in.format="%m/%d/%Y")[-1]
	)
# subset down to the range used in the text
sp500.logret.ts <- sp500.logret.ts[positions(sp500.logret.ts) <= timeDate("03/04/2003"),]

n <- nrow(sp500.logret.ts)
plot( seriesData(abs(sp500.logret.ts)), pch=".", type="p", axes=F, ylab="", 
	xlab="absolute log return", ylim=c(0,0.08), las=1 )
axis(1, at=par()$usr[1:2], labels=c("1993","2003"))
axis(2, at=seq(0,0.08,0.01), adj=1)
text(3, 0.075, "S & P 500", cex=2, adj=0)
lines(lowess(1:n, abs(sp500.logret.ts), f=1/15 ), lwd=4)
box()
