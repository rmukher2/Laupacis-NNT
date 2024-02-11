##################################################################################################################

#' @Description - Obtain the nonparametric CIs of Laupacis' NNT (Wald-based, 
#'                Delta based CI, BS based) for Setup - I, when the semiparametric model
#'                is misspecified.

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
library(boot)   # load library boot
tau = 100
p_c1   = function(data, indices) {
  p.c     = ifelse( mean( data[indices] > 100, na.rm = T ) < 0.001, # prob of success in the control grp
                    0.001,
                    mean( data[indices] > 100, na.rm = T ))
  var.pc  = var( data[indices] )                                       # variance of sampled control values
  mean.pc = mean( data[indices] )                                      # mean of sampled control values
  return(c(p.c, var.pc, mean.pc))
}

p_t1 = function(data, indices) {
  p.t     = ifelse( mean( data[indices] > 100, na.rm = T ) > 0.999, # prob of success in the treatment grp
                    0.999,
                    mean( data[indices] > 100, na.rm = T  ) )
  var.pt  = var( data[indices] )                                       # variance of sampled treatment values
  mean.pt = mean( data[indices] )                                      # mean of sampled treatment values
  return(c(p.t, var.pt, mean.pt))
}


n <- 800
I = 1000
treat <- list(mode="vector",length=I)
control <- list(mode="vector",length=I)
p_t <- numeric(I)
p_c <- numeric(I)
p_c.boot <- list(mode="vector",length=1000)
p_t.boot <- list(mode="vector",length=1000)
nntl.bs <- vector("list", I)
tau = 100
nntl <- numeric(I)
diff <- numeric(I)

sd.wald <- numeric(I)
sd.delta <- numeric(I)

cov1 <- numeric(I)
cov2 <- numeric(I)
cov3 <- numeric(I)
ci_w <- vector("list", I)
ci_d <- vector("list", I)
ci_bs <- vector("list", I)
Bias <- numeric(I)
ntl.bs <- numeric(I)


wald_diff <- numeric(I)
delta_diff <- numeric(I)
bs_diff <- numeric(I)

set.seed(123)
for (i in 1:I){
  treat[[i]] <- rrayleigh(3 * n, scale = 200)
  control[[i]] <- rrayleigh(n, scale = 110)
  p_t[i]<- mean( treat[[i]]   > tau, na.rm = T )
  p_c[i]<- mean( control[[i]]   > tau, na.rm = T ) 
  p_c.boot[[i]] = boot(data = control[[i]], statistic = p_c1, R = 1000)
  p_t.boot[[i]] = boot(data = treat[[i]],   statistic = p_t1, R = 1000)
  
  
  ## point est
  nntl[i]        = ifelse( 1 / ( p_t[i] - p_c[i] ) > 0,
                           1 / ( p_t[i] - p_c[i] ),
                           Inf )
  
  nntl.bs[[i]]     = ifelse( 1 / ( p_t.boot[[i]]$t[ ,1] - p_c.boot[[i]]$t[ ,1] ) > 0,
                             1 / ( p_t.boot[[i]]$t[ ,1] - p_c.boot[[i]]$t[ ,1] ),
                             Inf )
  

  n1 <- length(treat[[i]])
  n0 <- length(control[[i]])
  
  ##Wald's CI
  
  sd.wald[i]       = sqrt( p_t[i] * ( 1 - p_t[i] ) / n1 + p_c[i] * ( 1 - p_c[i] ) / n0 )


  ci_w[[i]]          =    c( max( 1 / (  p_t[i] - p_c[i] + qnorm(.975) * sd.wald[i] ), 1),
                             ifelse( 1 / (  p_t[i] - p_c[i] - qnorm(.975) * sd.wald[i] ) > 0,
                                     1 / (  p_t[i] - p_c[i] - qnorm(.975) * sd.wald[i] ),
                                     Inf ))

  # DELTA's CI
  sd.delta[i]      = ( 1 / (p_t[i] - p_c[i]) ^ 2 ) * sqrt(   p_t[i] * (1 - p_t[i]) / n1
                                                             + p_c[i] * (1 - p_c[i]) / n0 )
  ci_d[[i]]          =  c(max( nntl[i] - qnorm(.975) * sd.delta[i], 1), nntl[i] + qnorm(.975) * sd.delta[i])

  ci_bs[[i]]        = c( max(quantile(nntl.bs[[i]], .025), 1),  quantile(nntl.bs[[i]], .975) )
  
  ##Length of CI
  
  wald_diff[i] <- ci_w[[i]][2] - ci_w[[i]][1]
  delta_diff[i] <- ci_d[[i]][2] - ci_d[[i]][1]
  bs_diff[i] <- ci_bs[[i]][2] - ci_bs[[i]][1]
  
  ## Coverage
  
  cov1[i] <- ifelse(ci_w[[i]][1] <= 4.53 & ci_w[[i]][2] >= 4.53, 1, 0)
  cov2[i] <- ifelse(ci_d[[i]][1] <= 4.53 & ci_d[[i]][2] >= 4.53, 1, 0)
  cov3[i] <- ifelse(ci_bs[[i]][1] <= 4.53 & ci_bs[[i]][2] >= 4.53, 1, 0)
  
}
sum(is.infinite(unlist(nntl)))
nntl1 <- unlist(nntl[is.finite(unlist(nntl))])
median(nntl1)


median(wald_diff)                           # median of Wald's difference 
median(delta_diff[delta_diff!="NaN"])       # median of Delta difference
median(bs_diff)                             # median of BS difference
mean(cov1)                                  # Coverage prob of Wald based CI
mean(cov2, na.rm = TRUE)                    # Coverage prob of Delta based CI
mean(cov3)                                  # Coverage prob of BS based CI

### output for boxplot##
output <- cbind(n1,n0,wald_diff, delta_diff, bs_diff)
write.csv(output, "F:/Output/Setup 1/SP_MIS/Try_1205/NP/CI/T3C_CI_800.csv")
