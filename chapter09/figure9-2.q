############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure9-2.q                                              #
#                                                          #
############################################################

# read data
irates <- read.table("../common/data/WeeklyInterest.dat", skip=14, na.string="0")
names(irates) <- c("month","day","year","ff","tb03","cm10","cm30","discount","prime","aaa","unknown")

# convert to timeSeries object for a nice plot
# unsure of analogue in R (zoo?)
irates.ts <- timeSeries(irates[c("tb03","cm10","cm30")]/100, 
	positions=timeDate(paste(irates$month,irates$day,1900+irates$year,sep="/"),
		in.format="%m/%d/%Y", format="%02m/%02d/%4Y"))
irates.ts <- irates.ts[ timeDate("01/01/1978") <= positions(irates.ts), ]

# the next statement is only valid on Windows
trellis.device(graphsheet)
trellis.par.set("superpose.line",list(col=1:7,lty=c(1,3:8),lwd=rep(2,7)))
plot(irates.ts, reference.grid=F)
key(corner=c(1,1), border=T, 
	text=list(c("3 month T-Bill","10 year T-bond","30 year T-bond")),
	lines=Rows(trellis.par.get("superpose.line"),1:3))
