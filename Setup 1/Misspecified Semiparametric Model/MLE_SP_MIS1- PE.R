##################################################################################################################

#' @Description - Obtain the point estimates of Parametric Estimator of Laupacis' NNT for Setup - I
#'                when the semiparametric model is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param yc_bar Mean of the control arm
#' @param yt_bar Mean of the treatment arm
#' @param n_c number of observations in the control arm
#' @param n_t number of observations in the treatment arm


##################################################################################################################

library(VGAM) #load library VGAM
tau = 100
n <- 800
I <- 1000
treat <- list(mode="vector",length=I)
control <- list(mode="vector",length=I)
nnt.v <- vector("list", I)
coverage <- numeric(I)
yc_bar <- numeric(I)
yt_bar <- numeric(I)
s_ml <- numeric(I)
d <- numeric(I)
var_nnt.v <- numeric(I)
grad.nnt <- vector("list", I)
inv_fisher <- vector("list", I)
s_t <- list("vector", I)
s_c <- list("vector", I)
Bias <- numeric(I)

set.seed(123)
for (i in 1:I){
  treat[[i]] <- rrayleigh(3 * n, scale = 200)
  control[[i]] <- rrayleigh(n, scale = 110)
  
  yc_bar[i]  = mean( control[[i]] )
  yt_bar[i]  = mean( treat[[i]] )
  
  n0 <- length(control[[i]])
  n1 <- length(treat[[i]])
  
  s_ml[i]    = ( 1 / ( n0 + n1)  * ( (n0 - 1) * var( control[[i]] ) + (n1 - 1) * var( treat[[i]] ) ) ) ^ ( 1/2 )
  
  d[i]       = ( mean(treat[[i]]) - mean(control[[i]]) ) / s_ml[i]
  
  s_t[i]     = ( ( n1 - 1 ) * var( treat[[i]] ) /  n1 ) ^ ( 1 / 2 )
  
  s_c[i]     = ( ( n0 - 1 ) * var( control[[i]] ) / n0 ) ^ ( 1 / 2 )
  
  
 #point Estimate
  nnt.v[[i]]      = ifelse( (  pnorm( ( yt_bar[i] - tau ) / s_ml[i] )
                               - pnorm( ( yc_bar[i] - tau ) / s_ml[i] )  ) ^ ( - 1 )  > 0,
                            (  pnorm( ( yt_bar[i] - tau ) / s_ml[i] )
                               - pnorm( ( yc_bar[i] - tau ) / s_ml[i] )  ) ^ ( - 1 ),
                            Inf )
  
}

sum(is.infinite(unlist(nnt.v)))
nnt.v1 <- unlist(nnt.v[is.finite(unlist(nnt.v))])
Bias <- nnt.v1 - 4.53
est <- abs(mean(Bias))
mean(Bias)                              # Mean Bias of the estimator
sd_est <- sd(nnt.v1)
sd_est                                  # SD of the estimator
RMSE <- sqrt(mean((nnt.v1 - 4.53)^2))
RMSE                                    # RMSE of the estimator

bias_percent <- (est/sd_est)*100
bias_percent                            # Bias% of the estimator

### output for boxplot##
output <- cbind(n1,n0,nnt.v)
write.csv(output, "F:/Output/Setup 1/SP_MIS/Try_1205/Para/PE/T3C_800.csv")

