##################################################################################################################

#' @Description - Obtain the point estimates of nonparametric estimator of Laupacis' NNT 
#'                for Setup - I, when the parametric model is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param p_c sample proportion of success in the control arm
#' @param p_t sample proportion of success in the treatment arm
#' @param n_c number of observations in the control arm
#' @param n_t number of observations in the treatment arm

##################################################################################################################

n <- 800
I = 1000
treat <- list(mode="vector",length=I)
control <- list(mode="vector",length=I)
p_t <- numeric(I)
p_c <- numeric(I)
tau = 100
nntl <- numeric(I)

Bias <- numeric(I)

set.seed(123)
for (i in 1:I){
  treat[[i]] <- rexp(3 * n, rate = 0.0065)
  control[[i]] <- rexp(n, rate = 0.02)
  p_t[i]<- mean( treat[[i]]   > tau, na.rm = T )
  p_c[i]<- mean( control[[i]]   > tau, na.rm = T ) 
  
  
  ## point est
  nntl[i]        = ifelse( 1 / ( p_t[i] - p_c[i] ) > 0,
                           1 / ( p_t[i] - p_c[i] ),
                           Inf )
  

  n_t <- length(treat[[i]])
  n_c <- length(control[[i]])
  
}
sum(is.infinite(unlist(nntl)))
nntl1 <- unlist(nntl[is.finite(unlist(nntl))])
Bias <- nntl1 - 2.58
mean(Bias)                                  # Mean Bias of the estimator


est <- abs(mean(Bias))
sd_est <- sd(nntl1)
sd_est                                      # SD of the estimator
RMSE <- sqrt(mean((nntl1 - 2.58)^2))
RMSE                                        # RMSE of the estimator
bias_percent <- (est/sd_est)*100
bias_percent                                # Bias% of the estimator

### output for boxplot##
output <- cbind(n_t,n_c,nntl)
write.csv(output, "F:/Output/Setup 1/Misspecified Para/NonParametric/Point Estimate/T3C-PE_800.csv")

