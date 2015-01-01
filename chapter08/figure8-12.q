############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-12.q                                             #
#                                                          #
############################################################

# run figure8-11.q first

persp.setup(lty=c(1,1,1), col=c(1,1,2), lwd=c(2,0,1)) 
persp(k.seq, t.seq,
	matrix(predict(impvol.model, tk.frame, se.fit=T)$se.fit,nrow=nk,ncol=nt), 
	xlab="exercise price", ylab="time to maturity",
	zlab="se of implied volatility"	)
