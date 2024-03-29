##################################################################################################################

#' @Description - Obtain the Parametric CIs of Laupacis' NNT for Setup - II, when the semiparametric model
#'                is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param yc_bar Mean of the control arm
#' @param yt_bar Mean of the treatment arm
#' @param n_c number of observations in the control arm
#' @param n_t number of observations in the treatment arm
#' @param p_t.boot BS estimator of the sample proportion of success in the treatment arm
#' @param p_c.boot BS estimator of the sample proportion of success in the control arm

##################################################################################################################

library(boot) # load package boot
tau = log(3)
 p_c1   = function(data, indices) {
   p.c     = ifelse( mean( data[indices] > tau, na.rm = T ) < 0.001, # prob of success in the control grp
                     0.001,
                     mean( data[indices] > tau, na.rm = T ))
   var.pc  = var( data[indices] )                                       # variance of sampled control values
   mean.pc = mean( data[indices] )                                      # mean of sampled control values
   return(c(p.c, var.pc, mean.pc))
 }

 p_t1 = function(data, indices) {
   p.t     = ifelse( mean( data[indices] > tau, na.rm = T ) > 0.999, # prob of success in the treatment grp
                     0.999,
                     mean( data[indices] > tau, na.rm = T  ) )
   var.pt  = var( data[indices] )                                       # variance of sampled treatment values
   mean.pt = mean( data[indices] )                                      # mean of sampled treatment values
   return(c(p.t, var.pt, mean.pt))
 }

n <- 800
I <- 1000
treat <- list(mode="vector",length=I)
control <- list(mode="vector",length=I)
p_c.boot <- list(mode="vector",length=1000)
p_t.boot <- list(mode="vector",length=1000)
nnt.v.bs <- vector("list", I)
nnt.v <- vector("list", I)
ci.bs <- vector("list", I)
ci.d.mle <- vector("list", I)
bs_diff <- numeric(I)
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
s_t1 <- numeric(I)
s_c1 <- numeric(I)
s_ml.bs <- list(mode="vector",length=I)
s_t.bs <- list(mode="vector",length=I)
s_c.bs <- list(mode="vector",length=I)
diff1 <- numeric(I)
diff2 <- numeric(I)
c1 <- numeric(I)
c2 <- numeric(I)
Bias <- numeric(I)
nntl_fin <- numeric(I)


set.seed(123)
for (i in 1:I){
  treat[[i]] <- rgamma(n, shape = 1.2, rate = 1.5)
  control[[i]] <- rgamma(3 * n, shape = 0.5, rate = 1.5)

### Initial values
  yc_bar[i]  = mean( control[[i]] )
  yt_bar[i]  = mean( treat[[i]] )
  
  n_c <- length(control[[i]])
  n_t <- length(treat[[i]])

s_t[i]     = ( ( n_t - 1 ) * var( treat[[i]] ) /  n_t ) ^ ( 1 / 2 )
s_c[i]     = ( ( n_c - 1 ) * var( control[[i]] ) / n_c ) ^ ( 1 / 2 )
s_t1[i]    = unlist(s_t[i])
s_c1[i]    = unlist(s_c[i])

### Bottstrap estimators
#95% quantile BS confidence interval
p_c.boot[[i]] = boot(data = control[[i]], statistic = p_c1, R = 1000)
p_t.boot[[i]] = boot(data = treat[[i]],   statistic = p_t1, R = 1000)

### BOOTSTRAP ESTIMATORS ###

s_t.bs[[i]]     = ( ( n_t - 1 )   * p_t.boot[[i]]$t[, 2] / n_t ) ^ ( 1 / 2 )

s_c.bs[[i]]     = ( ( n_c - 1 )   * p_c.boot[[i]]$t[, 2] / n_c ) ^ ( 1 / 2 )

## Estimators

nnt.v[[i]]      = ifelse( ( exp( - yt_bar[i] ^ (-1) * tau ) - exp( - yc_bar[i] ^ (-1) * tau ) ) ^ (-1) > 1,
                     ( exp( - yt_bar[i] ^ (-1) * tau ) - exp( - yc_bar[i] ^ (-1) * tau ) ) ^ (-1),
                     Inf )

nnt.v.bs[[i]]   = ifelse( ( exp( - p_t.boot[[i]]$t[ ,3] ^ (-1) * tau ) - exp( - p_c.boot[[i]]$t[ ,3] ^ (-1) * tau ) ) ^ (-1) > 0,
                      ( exp( - p_t.boot[[i]]$t[ ,3] ^ (-1) * tau ) - exp( - p_c.boot[[i]]$t[ ,3] ^ (-1) * tau ) ) ^ (-1),
                      Inf )

 
# DELTA's CI
# gradient of nnt.v
 
grad.nnt[[i]]           = c(   nnt.v[[i]] ^ 2 * exp( - ( yt_bar[i] ) ^ (-1) * tau ) * tau,
                                              - nnt.v[[i]] ^ 2 * exp( - ( yc_bar[i] ) ^ (-1) * tau ) * tau )

# # inverse Fisher inf. matrix of lambda_t, lambda_c
inv_fisher[[i]]     =   diag( c( ( yt_bar[i] ) ^ (-2),
                             ( yc_bar[i] ) ^ (-2)  ) , 2 )

# # variance of nnt.v
var_nnt.v[i]      = t( grad.nnt[[i]] ) %*% inv_fisher[[i]] %*% grad.nnt[[i]] * ( n_c + n_t ) / ( 2 * n_t * n_c )

# # nnt.v delta CI
ci.d.mle[[i]]   = c( max( nnt.v[[i]] - 1.96 * sqrt( var_nnt.v[i] ), 1),
                 nnt.v[[i]] + 1.96 * sqrt( var_nnt.v[i] )   )

# # BS CI

ci.bs[[i]]      =  c( max( quantile(nnt.v.bs[[i]], .025), 1), quantile(nnt.v.bs[[i]], .975) )

## Length of CI
diff1[i] <- ci.d.mle[[i]][2] - ci.d.mle[[i]][1]
diff2[i] <- ci.bs[[i]][2] - ci.bs[[i]][1]
# Coverage 
c1[i] <- ifelse(ci.d.mle[[i]][1] <= 5.47 & ci.d.mle[[i]][2] >= 5.47, 1, 0)
c2[i] <- ifelse(ci.bs[[i]][1] <= 5.47 & ci.bs[[i]][2] >= 5.47, 1, 0)

#nntl_fin[i] <- unlist(nnt.v[[i]][is.finite(unlist(nnt.v[[i]]))])

}

sum(is.infinite(unlist(nnt.v)))

nntl_fin <- unlist(nnt.v[is.finite(unlist(nnt.v))])
median(diff1[diff1 != 'NaN'])                         # median of delta based CI
median(diff1)
median(diff2)                                         # median of BS based CI

mean(c1)
mean(c1, na.rm = TRUE)                                # coverage of delta based CI
mean(c2)                                              # coverage of BS based CI

# output for boxplot
output <- cbind(n_t,n_c, diff1, diff2)
write.csv(output, "F:/Output/Setup 2/SP_MIS/Try 2/Para/CI/C3T_800.csv")