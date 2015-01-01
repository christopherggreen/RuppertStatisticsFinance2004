############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page283.q                                                #
#                                                          #
############################################################

impvol <- read.table("../common/data/ImpVolRegData.txt",skip=5,header=F)
names(impvol) <- c("K","T.exp","ImpVol")
impvol$T.exp <- impvol$T.exp - 63.040
impvol$K     <- impvol$K - 46.7500

colMeans(impvol)

impvol$T2 <- impvol$T.exp^2
impvol$K2 <- impvol$K^2
impvol$KT <- impvol$K * impvol$T.exp
impvol$K3 <- impvol$K^3
impvol$T3 <- impvol$T.exp^3

# the leaps function is used to perform "best subset selection" regression
# method can be r2, Cp, adjr2
# but by default only prints out values for the selected method

impvol.model.subsets <- leaps( impvol[,c("K","K2","K3","T.exp","T2","T3","KT")], impvol$ImpVol,
	method="r2", nbest=2, names=c("K","K2","K3","T.exp","T2","T3","KT") )
# convert from percent
impvol.model.subsets$r2 <- round(impvol.model.subsets$r2/100,4)
data.frame(impvol.model.subsets[c("size","r2","label")])


