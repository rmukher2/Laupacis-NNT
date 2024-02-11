##################################################################################################################

#' @description - Point estimate and CI of Semiparametric estimator of Laupacis' NNT for Unadjusted PANSS dataset.

#' @source  Valentin Vancak, Yair Goldberg, and Stephen Z Levine. 
#'          Guidelines to understand and compute the number needed to
#'          treat. BMJ Ment Health, 24(4):131–136, 2021.
#'          
#' @param treatment a numeric vector of treatment results           
#' @param control   a numeric vector of control results
#' @param d  Indicator variable for Treatment/Control; control (d==0) .


##################################################################################################################

library(boot)         # Load Library boot
unadj <- read.csv("F:/Research/NNT/Dataset/Unadjusted PANSS/sample_dataset_unadj.csv", header = TRUE) # read the csv dataset from csv file
n1 <- length(unadj$treatment) # Number of subjects in the treatment group
n0 <- length(unadj$placebo)   # Number of subjects in the Control group
n <- n1 + n0
rho <- n1/n0
pi <- n1/n
 
t <- c(unadj$treatment,unadj$placebo)  
d <- ifelse(t==unadj$placebo,0,1)

dat2 <- data.frame(t,d)

tau = 0                             # MCID threshold value

mylogit <- glm(d~ t, family=binomial(link="logit"), data = dat2)


beta_c <- coef(mylogit)[2]
alpha_c <- ifelse(is.nan(log((1-pi)/pi)), coef(mylogit)[1], coef(mylogit)[1] + log((1-pi)/pi))

pi_c <- 1/(n0*(1 + rho*exp(alpha_c + t*beta_c)))
wt <- exp(alpha_c + t*beta_c)

p_c <- sum(ifelse(t <= tau, pi_c, 0))
p_t <- sum(ifelse(t <= tau, pi_c*wt, 0))


####### Point Estimate of Semiparametric Estimator of Laupacis' NNT #####
NNT_D <- ifelse(1/(p_t - p_c) > 0,
                1/(p_t - p_c),Inf)


###### Delta Based CI ##############
A0_t <- sum(ifelse(t <= tau,  pi_c *(exp(alpha_c + t*beta_c))/(1+ rho *exp(alpha_c + t*beta_c)),0))
A1_t <- sum(ifelse(t <= tau,  pi_c *(exp(alpha_c + t*beta_c)/(1+ rho *exp(alpha_c + t*beta_c)))*t
                   ,0))

A_0 <- sum(pi_c * (exp(alpha_c + t*beta_c))/(1+ rho * exp(alpha_c + t*beta_c)))
A_1 <- sum(pi_c *(exp(alpha_c + t*beta_c)/(1+ rho * exp(alpha_c + t*beta_c)))*t)
A_2 <- sum(pi_c *(exp(alpha_c + t*beta_c)/(1+ rho *exp(alpha_c + t*beta_c)))
           *t(t)*t)



A = matrix(c(A_0,A_1,t(A_1),A_2),nrow = 2,ncol = 2, byrow = TRUE)
A_inv <- solve(A)

ma1 <- matrix(c(A0_t,A1_t),nrow = 1, ncol = 2, byrow = T)
mul <- A0_t - (ma1 %*% A_inv %*%  t(ma1))

f_t<- mean( unadj$treatment   <= tau, na.rm = T )
g_t<- mean( unadj$placebo <= tau, na.rm = T ) 


NP_var <-  (  g_t * (1 - g_t) /n0 + f_t * (1 - f_t)/n1 )

SP_var <-  (1 + 1/rho)^2 *(rho /  n0) * mul

sdsp <- ( 1 / (p_t - p_c) ^ 2  ) *sqrt((NP_var - SP_var) )



ci_dr          =  c(max( NNT_D - qnorm(.975) * sdsp, 1), NNT_D + qnorm(.975) * sdsp)


######### Bootstrap based CI ########
logit.nnt <- function(data, indices) {
  data2 <- data[indices,]
  fit<-glm(d ~ t, data = data2, family = "binomial")  
  n1 <- length(which(data2$d==1))
  n0 <- length(which(data2$d==0))
  rho <- n1/n0
  n <- n1 + n0
  pi <- n1/n
  a1 <- coef(fit)[1] + log((1-pi)/pi)
  b1 <- coef(fit)[2]
  pi_c_boot <- 1/(n0*(1 + rho*exp(a1 + data2$t*b1)))
  wt_boot <- exp(a1 + data2$t*b1)
  
  
  p_c.boot <- sum(ifelse(data2$t <= tau, pi_c_boot, 0))
  p_t.boot <- sum(ifelse(data2$t <= tau, pi_c_boot*wt_boot, 0))
  
  NNT_D.boot <- ifelse(1/(p_t.boot - p_c.boot) > 0,
                       1/(p_t.boot - p_c.boot),Inf)
  
  
  return(NNT_D.boot)
} 

set.seed(123)
output <- boot(data=dat2, statistic=logit.nnt, 
               R=1000)
ci_bs.laup        = c( max(quantile(output$t, .025), 1),  quantile(output$t, .975) )

