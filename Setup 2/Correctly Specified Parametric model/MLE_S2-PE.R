##################################################################################################################

#' @Description - Obtain the point estimates of Parametric Estimator of Laupacis' NNT for Setup - II, when the 
#                 parametric model is correctly specified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param yt_bar Mean of the treatment arm
#' @param yc_bar Mean of the Control arm
#' @param n_c number of observations in the control arm
#' @param n_t number of observations in the treatment arm


##################################################################################################################



tau = log(3)

n <- 800
I <- 1000
treat <- list(mode="vector",length=I)
control <- list(mode="vector",length=I)
nnt.v <- vector("list", I)
yc_bar <- numeric(I)
yt_bar <- numeric(I)
s_ml <- numeric(I)
d <- numeric(I)
s_t <- list("vector", I)
s_c <- list("vector", I)
s_t1 <- numeric(I)
s_c1 <- numeric(I)
Bias <- numeric(I)
nntl_fin <- numeric(I)


set.seed(1000)
for (i in 1:I){
  treat[[i]] <- rexp(n,0.4)
  control[[i]] <- rexp(3 * n,1)

### Initial values
  yc_bar[i]  = mean( control[[i]] )
  yt_bar[i]  = mean( treat[[i]] )
  
  n_c <- length(control[[i]])
  n_t <- length(treat[[i]])

s_t[i]     = ( ( n_t - 1 ) * var( treat[[i]] ) /  n_t ) ^ ( 1 / 2 )
s_c[i]     = ( ( n_c - 1 ) * var( control[[i]] ) / n_c ) ^ ( 1 / 2 )
s_t1[i]    = unlist(s_t[i])
s_c1[i]    = unlist(s_c[i])


## Estimator

nnt.v[[i]]      = ifelse( ( exp( - yt_bar[i] ^ (-1) * tau ) - exp( - yc_bar[i] ^ (-1) * tau ) ) ^ (-1) > 1,
                     ( exp( - yt_bar[i] ^ (-1) * tau ) - exp( - yc_bar[i] ^ (-1) * tau ) ) ^ (-1),
                     Inf )

}

#unlist(nnt.v)
sum(is.infinite(unlist(nnt.v)))

nntl_fin <- unlist(nnt.v[is.finite(unlist(nnt.v))])
Bias <- nntl_fin -3.21

est <- abs(mean(Bias))                    
sd_est <- sd(nntl_fin)
sd_est                                    # SD of the estimator
RMSE <- sqrt(mean((nntl_fin - 3.21)^2))
RMSE                                      # RMSE of the estimator
bias_percent <- (est/sd_est)*100
bias_percent                              # Bias% of the estimator 
mean(Bias)                                # Mean Bias of the estimator

# Output for Boxplot
output <- cbind(n_t,n_c,nnt.v)
write.csv(output, "F:/Output/Setup 2/Para_Spec/Parametric/PE/C3T_800.csv")

