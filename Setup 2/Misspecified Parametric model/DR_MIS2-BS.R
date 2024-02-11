##################################################################################################################

#' @Description - Obtain the semiparametric Bootstrap CI of Laupacis' NNT 
#'                for Setup - II, when the parametric model is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param J   number of bootstrap iterations
#' @param rate_t rate of gamma distribution in the treatment arm
#' @param rate_c rate of gamma distribution in the control arm
#' @param shape_t shape parameter of the treatment arm
#' @param shape_c shape parameter of the control arm
#' @param pi_c weight function
#' @param n0 number of observations in the control arm
#' @param n1 number of observations in the treatment arm

##################################################################################################################


logit.fn <- function(data, tau=log(3)) {
  fit<-glm(d ~ t, data = data, family = "binomial")  
  n1 <- length(which(data$d==1))
  n0 <- length(which(data$d==0))
  rho <- n1/n0
  n <- n1 + n0
  pi <- n1/n
  a1 <- coef(fit)[1] + log((1-pi)/pi)
  b1 <- coef(fit)[2]
  pi_c <- 1/(n0*(1 + rho*exp(a1 + data$t*b1)))
  wt <- exp(a1 + data$t*b1)
  
  p_c <- sum(ifelse(data$t <= tau, pi_c, 0))
  p_t <- sum(ifelse(data$t<= tau, pi_c*wt, 0))
  A=  ifelse (1/(p_c-p_t) > 0 ,
              1/(p_c-p_t), Inf)
  #
  return(A)
}


simudata <- function(shape_t = 1.5, shape_c = 1.5, rate_t = 1, rate_c = 2, n0=50,n1=50){
  treat <- rgamma(n1, shape = shape_t, rate = rate_t)
  control <- rgamma(n0, shape = shape_c, rate = rate_c)
  
  
  
  t1=rbind(cbind(treat,1),cbind(control,0))
  colnames(t1)=c('t','d')
  # dat <- na.omit(data.frame(t,d))
  dat=as.data.frame(t1)
  return (dat)
}

logit.fn(simudata(n1= 50, n0 = 50))

btsimu<-function(I=1000,J=1000,shape_t = 1.5, shape_c = 1.5, rate_t = 1, rate_c = 2, n0=50,n1=50,seed1=123){
  
  res0=c()
  for (i in 1:I)
  {
    dat1=simudata(n1=n1,n0=n0)
    res1=c()
    for (j in 1:J){
      dat1bt=dat1[sample(1:(n1+n0), size = n1 + n0, replace = TRUE),]
      res1= c(res1, logit.fn(dat1bt))
    }
    ci1=c( max(quantile(res1, .025, na.rm = TRUE), 1),  quantile(res1, .975, na.rm = TRUE) )
    cid=c(ci1,ci1[2]-ci1[1])
    res0=rbind(res0,cid)
    
  }
  
  
  return(res0)
}

set.seed(123)
n_1 = 2400
n_0 = 800
results <- btsimu(n1= n_1, n0 = n_0)

bs_diff_dr <- results[,3]
cov <- ifelse(results[,1] <= 3.22 & results[,2] >= 3.22,1,0)
median(results[,3])          # median of BS difference
mean(cov)                    # Coverage of BS based CI

### output for boxplot##

output <- cbind(n_1,n_0,bs_diff_dr)
write.csv(output, "F:/Output/Setup 2/Para Misspecified/Semiparametric/BS-CI/T3C_CI_800.csv")
