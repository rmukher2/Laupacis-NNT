##################################################################################################################

#' @Description - Obtain the point estimates of Parametric Estimator of Laupacis' NNT for Setup - I, when the 
#                 parametric model is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param yc_bar Mean in the control arm
#' @param yt_bar Mean in the treatment arm
#' @param n_c number of observations in the control arm
#' @param n_t number of observations in the treatment arm


##################################################################################################################

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
  treat[[i]] <- rexp(3 * n , rate = 0.0065)
  control[[i]] <- rexp(n , rate = 0.02)
  
  yc_bar[i]  = mean( control[[i]] )
  yt_bar[i]  = mean( treat[[i]] )
  
  n_c <- length(control[[i]])
  n_t <- length(treat[[i]])
  
  s_ml[i]    = ( 1 / ( n_c + n_t)  * ( (n_c - 1) * var( control[[i]] ) + (n_t - 1) * var( treat[[i]] ) ) ) ^ ( 1/2 )
  
  d[i]       = ( mean(treat[[i]]) - mean(control[[i]]) ) / s_ml[i]
  
  s_t[i]     = ( ( n_t - 1 ) * var( treat[[i]] ) /  n_t ) ^ ( 1 / 2 )
  
  s_c[i]     = ( ( n_c - 1 ) * var( control[[i]] ) / n_c ) ^ ( 1 / 2 )
  
  
  ## Point Estimate ##
  nnt.v[[i]]      = ifelse( (  pnorm( ( yt_bar[i] - tau ) / s_ml[i] )
                               - pnorm( ( yc_bar[i] - tau ) / s_ml[i] )  ) ^ ( - 1 )  > 0,
                            (  pnorm( ( yt_bar[i] - tau ) / s_ml[i] )
                               - pnorm( ( yc_bar[i] - tau ) / s_ml[i] )  ) ^ ( - 1 ),
                            Inf )
  
}

sum(is.infinite(unlist(nnt.v)))
nnt.v1 <- unlist(nnt.v[is.finite(unlist(nnt.v))])
Bias <- nnt.v1 - 2.58
est <- abs(mean(Bias))
mean(Bias)                                 # Mean Bias of the estimator
sd_est <- sd(nnt.v1)
sd_est                                     # SD of the estimator
RMSE <- sqrt(mean((nnt.v1 - 2.58)^2))
RMSE                                       # RMSE of the estimator

bias_percent <- (est/sd_est)*100
bias_percent                              # Bias% of the estimator

### output for boxplot##

output <- cbind(n_t,n_c,nnt.v)
write.csv(output, "F:/Output/Setup 1/Misspecified Para/Parametric/PE/T3C_PE_800.csv")

