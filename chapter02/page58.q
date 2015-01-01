############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page58.q                                                 #
#                                                          #
############################################################
 
# S code equivalent to SAS code on page 58

# run page40.q first, so that ford data set is loaded

# compute returns	
return.ford <- diff(ford$ford)/ford$ford[-length(ford$ford)]

# compute 0.1 trimmed mean
mean(return.ford, trim=0.1)

# other stuff produced by proc univariate
summary(return.ford)
skewness(return.ford)
kurtosis(return.ford)
stdev(return.ford)

# tests of normality
shapiro.test(return.ford)
ks.gof(return.ford)
# cramer-von mises and anderson-darling are not available
# in s-plus

# qq plot (SAS code does it but Ruppert didn't include the figure)
# qqplot.matlab(return.ford, <add other arguments here>)
