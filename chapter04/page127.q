############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page127.q                                                #
#                                                          #
############################################################

# run figure4-7.q first to load the t-bill data
tbill.diff <- diff(tbill$Rate)
mean(tbill$Rate)
arima.mle(tbill$Rate, model=list(order=c(20,1,0)), max.iter=100, max.fcal=100)
