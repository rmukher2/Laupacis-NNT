##################################################################################################################

#' @description - Point estimate and CI of Semiparametric estimator of Laupacis' NNT for Wieand dataset.

#' @source  Wieand S, Gail MH, James BR, and James KL. A family of
#'          nonparametric statistics for comparing diagnostic markers with paired or
#'          unpaired data. Biometrika 76(3):585-92. 1989.
#'          
#' @param y1 CA 19-9            
#' @param y2 CA 125
#' @param d  Indicator variable for Pancreatic Cancer; non-cancer patients (d==0) are pancreatitis controls.


##################################################################################################################




library(boot) # Load Library boot 
wieand <- read.csv("F:/Research/NNT/Dataset/Wienand/wiedat2b.csv", header = TRUE) # read the csv dataset from csv file

n1 <- length(which(wieand$d==1)) # Number of subjects in the treatment group
n0 <- length(which(wieand$d==0)) # Number of subjects in the Control group
n <- n1 + n0
rho <- n1/n0
pi <- n1/n


t <- log(wieand$y1)

dat_w <- data.frame(wieand,t)

mylogit <- glm(d ~ t + I(t^2) , family=binomial(link="logit"), data = dat_w)


beta_c1 <- coef(mylogit)[2]
beta_c2 <- coef(mylogit)[3]
alpha_c <- ifelse(is.nan(log((1-pi)/pi)), coef(mylogit)[1], coef(mylogit)[1] + log((1-pi)/pi))

attach(dat_w)

pi_c <- 1/(n0*(1 + rho*exp(alpha_c + t*beta_c1 + t^2*beta_c2)))
wt <- exp(alpha_c + t*beta_c1 + t^2*beta_c2)


######### Laupacis Estimate of NNT and CI #################
## We assume the Cut-off to be 3 years ###
tau = 3
p_c <- sum(ifelse(t >= tau, pi_c, 0))
p_t <- sum(ifelse(t >= tau, pi_c*wt, 0))


### Semiparametric Point Estimate #####
NNT_D <- ifelse(1/(p_t - p_c) > 0,
                1/(p_t - p_c),Inf)


######### Delta based CI ######################
A0_t <- sum(ifelse(t >= tau,  pi_c *(exp(alpha_c + t*beta_c1 + t^2*beta_c2))/
                     (1+ rho *exp(alpha_c + t*beta_c1 + t^2*beta_c2)),0))


A1_t <- sum(ifelse(t >= tau,  pi_c *(exp(alpha_c + t*beta_c1 + t^2*beta_c2)/
                                       (1+ rho *exp(alpha_c + t*beta_c1 + t^2*beta_c2)))*t
                   ,0))


A_0 <- sum(pi_c * (exp(alpha_c + t*beta_c1 + t^2*beta_c2))/
             (1+ rho * exp(alpha_c + t*beta_c1 + t^2*beta_c2)))


A_1 <- sum(pi_c *(exp(alpha_c + t*beta_c1 + t^2*beta_c2)/
                    (1+ rho * exp(alpha_c + t*beta_c1 + t^2*beta_c2)))*t)


A_2 <- sum(pi_c *(exp(alpha_c + t*beta_c1 + t^2*beta_c2)/
                    (1+ rho *exp(alpha_c + t*beta_c1 + t^2*beta_c2)))*t(t)*t)



A = matrix(c(A_0,A_1,t(A_1),A_2),nrow = 2,ncol = 2, byrow = TRUE)
A_inv <- solve(A)

ma1 <- matrix(c(A0_t,A1_t),nrow = 1, ncol = 2, byrow = T)
mul <- A0_t - (ma1 %*% A_inv %*%  t(ma1))

f_t<- mean( dat_w$treat   >= tau, na.rm = TRUE )
g_t<- mean( placebo >= tau, na.rm = TRUE ) 


NP_var <-  (  g_t * (1 - g_t) /n0 + f_t * (1 - f_t)/n1 )

SP_var <-  (1 + 1/rho)^2 *(rho /  n0) * mul

sdsp <- ( 1 / (p_t - p_c) ^ 2  ) *sqrt((NP_var - SP_var) )



ci_dr          =  c(max( NNT_D - qnorm(.975) * sdsp, 1), NNT_D + qnorm(.975) * sdsp)

############ Bootstrap Based CI #####################

laup_wien <- function(data, indices) {
  data2 <- data[indices,]
  mylogit <- glm(d ~ t + I(t^2) , family=binomial(link="logit"), data = data2)
  n1 <- length(which(data2$d==1))
  n0 <- length(which(data2$d==0))
  rho <- n1/n0
  n <- n1 + n0
  pi <- n1/n
  
  beta_c1 <- coef(mylogit)[2]
  beta_c2 <- coef(mylogit)[3]
  alpha_c <- ifelse(is.nan(log((1-pi)/pi)), coef(mylogit)[1], coef(mylogit)[1] + log((1-pi)/pi))
  
  pi_c <- 1/(n0*(1 + rho*exp(alpha_c + data2$t*beta_c1 + data2$t^2*beta_c2)))
  wt <- exp(alpha_c + data2$t*beta_c1 + data2$t^2*beta_c2)
  
  p_c <- sum(ifelse(data2$t >= tau, pi_c, 0))
  p_t <- sum(ifelse(data2$t >= tau, pi_c*wt, 0))
  
  NNT_D <- ifelse(1/(p_t - p_c) > 0,
                  1/(p_t - p_c),Inf)
  
  return(NNT_D)
} 


laup_wien(data = dat_w)



set.seed(123)
laup_boot <- boot(data=dat_w, statistic=laup_wien, 
                  R=1000)


ci_l_bs        = c(quantile(laup_bbot$t[,1], .025),  quantile(laup_boot$t[,1], .975) )

