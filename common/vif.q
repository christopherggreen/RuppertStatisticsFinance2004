##################################################
#
# a very simple variance inflation calculator
#
# Posted by Bill Venables to the S-News and R-Help 
# mailing lists
# October 22, 2002
#
##################################################

# uses S3 classes
vif <- function(object, ...)
UseMethod("vif")

vif.default <- function(object, ...)
stop("No default method for vif.  Sorry.")

vif.lm <- function(object, ...) {       
  V <- summary(object)$cov.unscaled
  Vi <- crossprod(model.matrix(object))
  nam <- names(coef(object))
  if(k <- match("(Intercept)", nam, nomatch = F)) {
                v1 <- diag(V)[-k]
                v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
                nam <- nam[-k]
        } else {
                v1 <- diag(V)
                v2 <- diag(Vi)
                warning("No intercept term detected.  Results may
surprise.")
        }
        structure(v1*v2, names = nam)
}
