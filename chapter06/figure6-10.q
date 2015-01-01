############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-10.q                                             #
#                                                          #
############################################################

# run figure 6-8.q first

plot(x2, fit.diag$stud.res, las=1, type="p", ylab="std. residuals")
# add spline smoother
lines(lowess(x2,fit.diag$stud.res))


