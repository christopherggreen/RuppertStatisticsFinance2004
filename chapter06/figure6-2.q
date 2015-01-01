############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-2.q                                              #
#                                                          #
############################################################

# load weekly interest data
WeeklyInterest <- read.table("../common/data/WeeklyInterest.dat", 
	skip=14, na.string="0")
names(WeeklyInterest) <- c("month","day","year","ff","tb03","cm10",
	"cm30","discount","prime","aaa","unknown")

 aaa.dif <- diff(WeeklyInterest$aaa)
cm10.dif <- diff(WeeklyInterest$cm10)
fit <- lm(aaa.dif ~ cm10.dif, na.action=na.exclude)
anova(fit)
summary(fit)
# SAS-like output for comparison
# load the file summary.proc.reg.q
#
summary.proc.reg(fit)

plot(cm10.dif, aaa.dif, type="p", pch="x", 
	xlab="change in 10YR T rate",
	ylab="change in AAA rate")
