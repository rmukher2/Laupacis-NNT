##################################################################################################################

#' @Description - Obtain the point estimates of nonparametric estimator of Laupacis' NNT and Wald-based, 
#'                Delta based CI for Setup - I, when the parametric model is correctly specified.

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
treat[[i]] <- rnorm(3 * n,mean = 110, sd = 10)
control[[i]] <- rnorm(n,mean = 100, sd = 10)
n_t <- length(treat[[i]])
n_c <- length(control[[i]])
p_t[i]<- mean( treat[[i]]   > tau, na.rm = T )
p_c[i]<- mean( control[[i]]   > tau, na.rm = T ) 


## point est
nntl[i]        = ifelse( 1 / ( p_t[i] - p_c[i] ) > 0,
                         1 / ( p_t[i] - p_c[i] ),
                         Inf )

}

for (i in 1:I){

Bias[i] <- nntl[i] - 2.93

n_t <- length(treat[[i]])
n_c <- length(control[[i]])

##Wald's CI

sd.wald[i]       = sqrt( p_t[i] * ( 1 - p_t[i] ) / n_t + p_c[i] * ( 1 - p_c[i] ) / n_c )


ci_w[[i]]          =    c( max( 1 / (  p_t[i] - p_c[i] + qnorm(.975) * sd.wald[i] ), 1),
                      ifelse( 1 / (  p_t[i] - p_c[i] - qnorm(.975) * sd.wald[i] ) > 0,
                              1 / (  p_t[i] - p_c[i] - qnorm(.975) * sd.wald[i] ),
                              Inf ))

ci_w1[[i]]          =   c(max( nntl[i] - qnorm(.975) * sd.wald[i], 1), nntl[i] + qnorm(.975) * sd.wald[i])  

# DELTA based CI
sd.delta[i]      = ( 1 / (p_t[i] - p_c[i]) ^ 2 ) * sqrt(   p_t[i] * (1 - p_t[i]) / n_t
                                                           + p_c[i] * (1 - p_c[i]) / n_c )


ci_d[[i]]          =  c(max( nntl[i] - qnorm(.975) * sd.delta[i], 1), nntl[i] + qnorm(.975) * sd.delta[i]) 


###Length of CI
wald_diff[i] <- ci_w[[i]][2] - ci_w[[i]][1]
delta_diff[i] <- ci_d[[i]][2] - ci_d[[i]][1]


## Coverage
cov_wald[i] <- ifelse(ci_w[[i]][1] <= 2.93 & ci_w[[i]][2] >= 2.93, 1, 0)
cov_dl[i] <- ifelse(ci_d[[i]][1] <= 2.93 & ci_d[[i]][2] >= 2.93, 1, 0)

}
sum(is.infinite(unlist(nntl)))
nntl1 <- unlist(nntl[is.finite(unlist(nntl))])
Bias <- nntl1 - 2.93
mean(Bias)                                      # Mean Bias of the estimator


est <- abs(mean(Bias))                           
sd_est <- sd(nntl1)
sd_est                                           # SD of the estimator
RMSE <- sqrt(mean((nntl1 - 2.93)^2))
RMSE                                             # RMSE of the estimator
bias_percent <- (est/sd_est)*100
bias_percent                                     # Bias% of the estimator
median(wald_diff)                                # median of wald difference
median(delta_diff)                               # median of delta difference


median(delta_diff[delta_diff!="NaN"])
mean(cov_wald)                                   # Coverage of Wald's CI
mean(cov_dl)                                     # Coverage of delta-based CI

### output for boxplot##
output <- cbind(n_t,n_c,nntl,wald_diff,delta_diff)
write.csv(output, "F:/Output/Setup 1/Para_Spec/NonParametric/Point Estimate/T3C_PE_800.csv")
