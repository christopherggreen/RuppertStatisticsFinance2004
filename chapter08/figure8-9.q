############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-9.q                                              #
#                                                          #
############################################################

# run page283.q first

# figure 8.9
# fit final model
impvol.model <- lm ( ImpVol ~ K + K2 + T.exp + T2 + T3, data=impvol )

summary.proc.reg(impvol.model)

anova(impvol.model)
summary(impvol.model)
coef(impvol.model)

# create a data frame containing the values at which we want to 
# predict from the model
const.t.frame <- data.frame( 
	outer(seq(35,55,1)-46.7500,c(1,2),FUN=function(x,y) x^y), 
	outer(rep(0,21),c(1,2,3),FUN=function(x,y) x^y)
)
names(const.t.frame) <- c("K","K2","T.exp","T2","T3")
par(lwd=2,xaxs="i",yaxs="i",las=1)
plot(const.t.frame$K+46.7500, predict(impvol.model, const.t.frame), 
	type="l", xlab="exercise price",ylab="implied volatility",
	xlim=c(35,55),ylim=c(0.018,0.03))
	
		
