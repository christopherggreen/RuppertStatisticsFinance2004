############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page40.q                                                 #
#                                                          #
############################################################
 
# S code equivalent to SAS code on page 40

# the data for this example is  
# available on Ruppert's web site
# in the file capm.txt

# read.table is the easiest way to load the data from
# the file. there are other arguments that control 
# the field separator, etc.
#
# row.names prevents R/S-Plus from interpreting one of the
# fields as row names
#
# as.is keeps character data as character and not factor
#
ford <- read.table("../common/data/capm.txt", skip=10, row.names=NULL,as.is=T)
names(ford) <- c("tbill","microsoft","sp500","ge","ford","day","month","year")

# compute returns	
return.sp500 <- diff(ford$sp500)/ford$sp500[-length(ford$sp500)]
return.ge    <- diff(ford$ge)/ford$ge[-length(ford$ge)]
return.ford  <- diff(ford$ford)/ford$ford[-length(ford$ford)]

# cbind joins vectors (interpreted as columns) together into a matrix
# compute correlation matrix
cor(cbind(return.sp500, return.ge, return.ford))
# compute covariance matrix
var(cbind(return.sp500, return.ge, return.ford))
