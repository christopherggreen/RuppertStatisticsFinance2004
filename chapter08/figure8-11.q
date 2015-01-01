############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-11.q                                             #
#                                                          #
############################################################

# run page283.q first
# run figure8-9.q first
# figure 8.11

k.seq <- seq(35,55,1)
t.seq <- seq(0,150,5)
nk <- length(k.seq)
nt <- length(t.seq)

# this is a data frame containing values at which 
# we want to predict from the model
# there are three dimensions here: k, t, and vhat
# but a data frame is a 2D table, so we stack things
# we'll arrange the output predict() to a matrix
# suitable for persp

tk.frame <- data.frame( 
	rep(k.seq-47.500,nt),
	rep(k.seq-47.500,nt)^2,
	rep(t.seq-63.040,each=nk),
	rep(t.seq-63.040,each=nk)^2,
	rep(t.seq-63.040,each=nk)^3
)
names(tk.frame) <- c("K","K2","T.exp","T2","T3")

persp.setup(lty=c(1,1,1), col=c(1,1,2), lwd=c(2,0,1)) 
persp(k.seq, t.seq,
	matrix(predict(impvol.model, tk.frame),nrow=nk,ncol=nt), 
	xlab="exercise price", ylab="time to maturity",
	zlab="implied volatility",
	zlim=c(0.015,0.04))
	

	
		
