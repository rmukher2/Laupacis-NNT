##################################################################################################################

#' @Description - Obtain the BS CI for Nonparametric Laupacis NNT for Setup - II, 
#'                when the parametric model is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param p_c sample proportion of success in the control arm
#' @param p_t sample proportion of success in the treatment arm
#' @param n_c number of observations in the control arm
#' @param n_t number of observations in the treatment arm
#' @param p_t.boot BS estimator of the sample proportion of success in the treatment arm
#' @param p_c.boot BS estimator of the sample proportion of success in the control arm

##################################################################################################################

tau = log(3)
library(boot) # load package boot
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

# 95% quantile BS confidence interval
n <- 800
I <- 1000
treat <- list(mode="vector",length=I)
control <- list(mode="vector",length=I)
p_c.boot <- list(mode="vector",length=1000)
p_t.boot <- list(mode="vector",length=1000)
nntl.bs <- vector("list", I)
ci_bs <- vector("list", I)
bs_diff <- numeric(I)
coverage <- numeric(I)

set.seed(123)
for (i in 1:I){
treat[[i]] <- rgamma(n,shape = 1.5, rate = 1)
control[[i]] <- rgamma(3 * n,shape = 1.5, rate = 2)

n_c <- length(control[[i]])
n_t <- length(treat[[i]])

p_c.boot[[i]] = boot(data = control[[i]], statistic = p_c1, R = 1000)
p_t.boot[[i]] = boot(data = treat[[i]],   statistic = p_t1, R = 1000)

nntl.bs[[i]]     = ifelse( 1 / ( p_t.boot[[i]]$t[ ,1] - p_c.boot[[i]]$t[ ,1] ) > 0,
                         1 / ( p_t.boot[[i]]$t[ ,1] - p_c.boot[[i]]$t[ ,1] ),
                         Inf )
##Bootstrap CI
ci_bs[[i]]        = c( max(quantile(nntl.bs[[i]], .025), 1),  quantile(nntl.bs[[i]], .975) )
##Length of CI
bs_diff[i] <- ci_bs[[i]][2] - ci_bs[[i]][1]
## Coverage
coverage[i] <- ifelse(ci_bs[[i]][1] <= 3.22 & ci_bs[[i]][2] >= 3.22, 1, 0)
}
median(bs_diff)      # median of BS CI
mean(coverage)       # coverage of BS CI  

### output for boxplot##
output <- cbind(n_t,n_c,bs_diff)
write.csv(output, "F:/Output/Setup 2/Para Misspecified/NonParametric/BS - CI/C3T_CI_800.csv")

