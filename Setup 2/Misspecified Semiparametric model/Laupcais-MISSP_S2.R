##################################################################################################################

#' @Description - Obtain the point estimates of nonparametric estimator of Laupacis' NNT and Wald-based, 
#'                Delta based CI for Setup - II, when the semiparametric model is misspecified.

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
tau = log(3)
nntl <- list(mode="vector",length=I)
nntl_fin <- list(mode="vector",length=I)
diff <- numeric(I)

sd.wald <- numeric(I)
sd.delta <- numeric(I)

cov1 <- numeric(I)
cov2 <- numeric(I)
ci_w <- vector("list", I)
ci_d <- vector("list", I)
ci_bs <- vector("list", I)
Bias <- numeric(I)
ntl.bs <- numeric(I)


wald_diff <- numeric(I)
delta_diff <- numeric(I)

set.seed(1000)
for (i in 1:I){
treat[[i]] <- rgamma(n, shape = 1.2, rate = 1.5)
control[[i]] <- rgamma(3 * n, shape = 0.5, rate = 1.5)
p_t[i]<- mean( treat[[i]]   > tau, na.rm = T )
p_c[i]<- mean( control[[i]]   > tau, na.rm = T ) 

## point est
nntl[[i]]        = ifelse( 1 / ( p_t[[i]] - p_c[[i]] ) > 0,
                         1 / ( p_t[[i]] - p_c[[i]] ),
                         Inf )

}

nntl_fin <- unlist(nntl[is.finite(unlist(nntl))])

sum(is.infinite(unlist(nntl_fin)))

for (i in 1:I){
  
Bias <- nntl_fin - 5.47



n_t <- length(treat[[i]])
n_c <- length(control[[i]])

##Wald's CI

sd.wald[i]       = sqrt( p_t[i] * ( 1 - p_t[i] ) / n_t + p_c[i] * ( 1 - p_c[i] ) / n_c )


ci_w[[i]]          =    c( max( 1 / (  p_t[i] - p_c[i] + qnorm(.975) * sd.wald[i] ), 1),
                      ifelse( 1 / (  p_t[i] - p_c[i] - qnorm(.975) * sd.wald[i] ) > 0,
                              1 / (  p_t[i] - p_c[i] - qnorm(.975) * sd.wald[i] ),
                              Inf ))

# DELTA's CI
sd.delta[i]      = ( 1 / (p_t[i] - p_c[i]) ^ 2 ) * sqrt(   p_t[i] * (1 - p_t[i]) / n_t
                                                           + p_c[i] * (1 - p_c[i]) / n_c )
ci_d[[i]]          =  c(max( nntl[[i]] - qnorm(.975) * sd.delta[i], 1), nntl[[i]] + qnorm(.975) * sd.delta[i])

# Length of CI
wald_diff[i] <- ci_w[[i]][2] - ci_w[[i]][1]
delta_diff[i] <- ci_d[[i]][2] - ci_d[[i]][1]

# Coverage
cov1[i] <- ifelse(ci_w[[i]][1] <= 5.47 & ci_w[[i]][2] >= 5.47, 1, 0)
cov2[i] <- ifelse(ci_d[[i]][1] <= 5.47 & ci_d[[i]][2] >= 5.47, 1, 0)

}

sum(is.infinite(unlist(nntl_fin)))


est <- abs(mean(Bias))
#nntl1 <- unlist(nntl)
sd_est <- sd(nntl_fin)
sd_est                                             # SD of the estimator
RMSE <- sqrt(mean(Bias)^2 + sd_est^2)
RMSE                                               # RMSE of the estimator
bias_percent <- (est/sd_est)*100
bias_percent                                       # Bias % of the estimator
mean(Bias)                                         # mean Bias of the estimator
#median(wald_diff)
sum(is.na(delta_diff))
median(wald_diff[wald_diff != 'Inf'])              # median of wald difference
median(delta_diff[delta_diff != 'NaN'])            # median of Delta difference
mean(cov1)                                         # coverage of Wald's CI                             
mean(cov2, na.rm = TRUE)                           # Coverage of delta-based CI

### output for boxplot##
 output <- cbind(n_t,n_c,nntl,wald_diff,delta_diff)
 write.csv(output, "F:/Output/Setup 2/SP_MIS/Try 2/Nonpara/PE/C3T_800.csv")



