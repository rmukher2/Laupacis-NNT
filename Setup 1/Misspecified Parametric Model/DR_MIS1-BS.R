##################################################################################################################

#' @Description - Obtain the semiparametric Bootstrap CI of Laupacis' NNT 
#'                for Setup - I, when the parametric model is misspecified.

#' @param treat a numeric vector of the treatment arm results
#' @param control a numeric vector of the control arm results
#' @param tau cut-off value
#' @param I   number of iterations
#' @param J   number of bootstrap iterations
#' @param rate_t Rate of the treatment arm
#' @param rate_c Rate of the control arm
#' @param pi_c weight function
#' @param n0 number of observations in the control arm
#' @param n1 number of observations in the treatment arm

##################################################################################################################

expit.boot <- function(data, tau = 100){

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
p_t <- sum(ifelse(data$t <= tau, pi_c*wt, 0))
A=1/(p_c-p_t)

return(A)
}

dt_fun <- function(rate_t = 0.0065, rate_c = 0.02, n1 = 50, n0 = 50){
treat <- rexp(n1,rate = rate_t)
control <- rexp(n0, rate = rate_c)
t1=rbind(cbind(treat,1),cbind(control,0))
colnames(t1)=c('t','d')
# dat <- na.omit(data.frame(t,d))
dat=as.data.frame(t1)
return (dat)
}

expit.boot(dt_fun())

btsimu_exp<-function(I=1000,J=1000,n1=50,n0=50,rate_t = 0.0065, rate_c = 0.02,seed1=1000){
  res0=c()
  for (i in 1:I)
  {
    dat1=dt_fun(n1=n1,n0=n0)
    res1=c()
    for (j in 1:J){
      dat1bt=dat1[sample(1:(n1+n0), size = n1 +n0, replace = TRUE),]
      res1= c(res1, expit.boot(dat1bt))
    }
    ci1=c( max(quantile(res1, .025), 1),  quantile(res1, .975) )
    cid=c(ci1,ci1[2]-ci1[1])
    res0=rbind(res0,cid)
    
  }
  
  
  return(res0)
}

set.seed(123)
n_1 = 600
n_0 = 200
mm <- btsimu_exp(n1 = n_1, n0 = n_0)
bs_diff_dr <- mm[,3]
cov <- ifelse(mm[,1] <= 2.58 & mm[,2] >= 2.58,1,0)
median(mm[,3])                     # median of BS difference
mean(cov)                          # Coverage prob of BS based CI

### output for boxplot##
output <- cbind(n_1,n_0,bs_diff_dr)
write.csv(output, "F:/Output/Setup 1/Misspecified Para/Semiparametric/BS-CI/T3C_CI_200.csv")