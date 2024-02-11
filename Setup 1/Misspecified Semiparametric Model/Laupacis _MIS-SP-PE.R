##################################################################################################################

#' @Description - Obtain the point estimates of nonparametric estimator of Laupacis' NNT 
#'                when the semiparametric model is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param p_c sample proportion of success in the control arm
#' @param p_t sample proportion of success in the treatment arm
#' @param n_c number of observations in the control arm
#' @param n_t number of observations in the treatment arm

##################################################################################################################
library(VGAM)   # load library VGAM
n <- 800
I = 1000
treat <- list(mode="vector",length=I)
control <- list(mode="vector",length=I)
p_t <- numeric(I)
p_c <- numeric(I)
tau = 100
nntl <- numeric(I)
diff <- numeric(I)

sd.wald <- numeric(I)
sd.delta <- numeric(I)

cov_wald <- numeric(I)
cov_dl <- numeric(I)
ci_w <- vector("list", I)
ci_d <- vector("list", I)
ci_bs <- vector("list", I)
ci_w1 <- vector("list", I)
ddd <- vector("list", I)
Bias <- numeric(I)
ntl.bs <- numeric(I)


wald_diff <- numeric(I)
delta_diff <- numeric(I)

set.seed(123)
for (i in 1:I){
  treat[[i]] <- rrayleigh(3 * n, scale = 200)
  control[[i]] <- rrayleigh(n, scale = 110)
  n1 <- length(treat[[i]])
  n0 <- length(control[[i]])
  p_t[i]<- mean( treat[[i]]   > tau, na.rm = T )
  p_c[i]<- mean( control[[i]]   > tau, na.rm = T ) 
  
  
  ## point est
  nntl[i]        = ifelse( 1 / ( p_t[i] - p_c[i] ) > 0,
                           1 / ( p_t[i] - p_c[i] ),
                           Inf )
  
}
sum(is.infinite(unlist(nntl)))
nntl1 <- unlist(nntl[is.finite(unlist(nntl))])
Bias <- nntl1 - 4.53
mean(Bias)                                  # Mean Bias of the estimator

est <- abs(mean(Bias))                       
sd_est <- sd(nntl1)
sd_est                                       # SD of the estimator
RMSE <- sqrt(mean((nntl1 - 4.53)^2))
RMSE                                         # RMSE of the estimator
bias_percent <- (est/sd_est)*100
bias_percent                                 # Bias% of the estimator
median(wald_diff)



### output for boxplot##
output <- cbind(n1,n0,nntl,wald_diff,delta_diff)
write.csv(output, "F:/Output/Setup 1/SP_MIS/Try_1205/NP/PE/T3C_800.csv")

