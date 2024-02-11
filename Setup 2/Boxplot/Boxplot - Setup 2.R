##################################################################################################################

#' @Description - This R program creates boxplots of Nonparametric, Parametric and Semiparametric
#                 estimator of Laupacis'NNT. In this program, boxplots of both point estimator and   
#                 CIs are constructed for Setup - II (Correctly Specified Parametric Model,
#                 Misspecified Parametric Model and Misspecified Semiparametric Model) for different
#                 sample size considerations.

##################################################################################################################


install.packages("reshape2")                                 # Install reshape2 package
install.packages("cowplot")                                  # Install cowplot package
install.packages("ggpubr")                                   # Install ggpubr package
library(ggpubr)                                              # Load ggpubr
library(reshape2)                                            # Load reshape2
library(cowplot)                                             # Load cowplot
library(dplyr)                                               # Load dplyr          
library(ggplot2)                                             # Load ggplot2

###################################### SETUP 2 ##############################################
####################### CORRECTLY SPECIFIED PARAMETRIC MODEL ################
#####################    EQUAL SAMPLE SIZE     #################################
para_eq <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Parametric Specified\\Para-Spec-S2-EQ.csv", header = TRUE)
para_eq <- transform(para_eq, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ### 
para_eq_pe <-  select(para_eq, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 

### Dataset for CI ###
para_eq_CI <- select(para_eq, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                        NNT_LP_DL, NNT_LP_BS, n)



data_long_p_pe <- melt(para_eq_pe, id = c("n1","n0", "n")) 
data_long_p_CI <- melt(para_eq_CI, id = c("n1","n0", "n"))
head(data_long_p_pe)
head(data_long_p_CI)


#### Box-Plot of Point Estimates #########
data_long_p_pe <- subset(data_long_p_pe, subset = value < 10)
p_eq_pe <- ggplot(data_long_p_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 3.21, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Equal Sample Size")+
  xlab("")+
  ylab("NNT") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p_eq_pe)


#### Box-Plot of CIs #########
data_long_p_CI <- subset(data_long_p_CI, subset = value < 10)
p_eq_CI <- ggplot(data_long_p_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Equal Sample Size")+
  xlab("")+
  ylab("95% Confidence Interval") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p_eq_CI)


#######################  TREATMENT ARM HAVE SAMPLE SIZE 3 TIMES CONTROL ARM  #################################
para_T3C <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Parametric Specified\\Para-Spec-S2-T3C.csv", header = TRUE)
para_T3C <- transform(para_T3C, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_T3C_pe <- select(para_T3C, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_T3C_CI <- select(para_T3C, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                      NNT_LP_DL, NNT_LP_BS, n)


data_pT3C_pe <- melt(para_T3C_pe, id = c("n1","n0", "n")) 
data_pT3C_CI <- melt(para_T3C_CI, id = c("n1","n0", "n"))


#### Box-Plot of Point Estimates #########
data_pT3C_pe <- subset(data_pT3C_pe, subset = value < 10)
p_T3C_pe <- ggplot(data_pT3C_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
            geom_boxplot() +
            geom_hline(yintercept = 3.21, linetype = "solid", col = "red", linewidth = 0.75)     +
            ggtitle("Treatment 3 times Control") +
            xlab("")+
            ylab(" ") +
            scale_fill_discrete(name = "Estimators")+
            scale_y_continuous(breaks = 1:15)  +
            theme_minimal()+
            coord_cartesian(ylim = c(2, 10))+
            theme(legend.position = "bottom") +
            theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
print(p_T3C_pe)


#### Box-Plot of CIs #########
data_pT3C_CI <- subset(data_pT3C_CI, subset = value < 10)
p_T3C_CI <- ggplot(data_pT3C_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Treatment 3 times Control") +
  xlab(" ")+
  ylab(" ") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p_T3C_CI)


#######################  CONTROL ARM HAVE SAMPLE SIZE 3 TIMES TREATMENT ARM  #################################
para_C3T <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Parametric Specified\\Para-Spec-S2-C3T.csv", header = TRUE)
para_C3T <- transform(para_C3T, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_C3T_pe <- select(para_C3T, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_C3T_CI <- select(para_C3T, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                       NNT_LP_DL, NNT_LP_BS, n)



data_pC3T_pe <- melt(para_C3T_pe, id = c("n1","n0", "n")) 
data_pC3T_CI <- melt(para_C3T_CI, id = c("n1","n0", "n"))
head(data_pC3T_pe)
head(data_pC3T_CI)


#### Box-Plot of Point Estimates #########
data_pC3T_pe <- subset(data_pC3T_pe, subset = value < 10)
p_C3T_pe <- ggplot(data_pC3T_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 3.21, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Control 3 times Treatment")+
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p_C3T_pe)


#### Box-Plot of CIs #########
data_pC3T_CI <- subset(data_pC3T_CI, subset = value < 10)
p_C3T_CI <- ggplot(data_pC3T_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Control 3 times Treatment")+
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p_C3T_CI)


############################################ COMBINED BOX-PLOTS #######################################
############### POINT ESTIMATE ###############
para_pe <- ggarrange(p_eq_pe, p_T3C_pe, p_C3T_pe, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 3)  # number of rows
print(para_pe)
ggsave("F:/Output/Boxplots/Setup - 2/Para-Specified/Para-S2_PE.jpeg", height = 7 , width = 7 * 2)

############ CONFIDENCE INTERVAL ############
para_CI <- ggarrange(p_eq_CI, p_T3C_CI, p_C3T_CI, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 3)  # number of rows
print(para_CI)
ggsave("F:/Output/Boxplots/Setup - 2/Para-Specified/Para-S2_CI.jpeg", height = 7 , width = 7 * 2)



################################################################################################################################
############################################## MISSPECIFIED PARAMETRIC MODEL ###################################################
######################### EQUAL SAMPLE SIZE ##############################
para_mis_eq <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Parametric MisSpecified\\Para-MIS-S2-EQ.csv", header = TRUE)
para_mis_eq <- transform(para_mis_eq, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_mis_eq_pe <- select(para_mis_eq, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_mis_eq_CI <- select(para_mis_eq, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                         NNT_LP_DL, NNT_LP_BS, n)


data_pmiss_eq_pe <- melt(para_mis_eq_pe, id = c("n1","n0", "n")) 
data_pmiss_eq_CI <- melt(para_mis_eq_CI, id = c("n1","n0", "n"))
head(data_pmiss_eq_pe)
head(data_pmiss_eq_CI)


#### Box-Plot of Point Estimates #########
data_pmiss_eq_pe <- subset(data_pmiss_eq_pe, subset = value < 10)
pmis_eq_pe <- ggplot(data_pmiss_eq_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 3.22, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Equal Sample Size") +
  xlab("")+
  ylab("NNT") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pmis_eq_pe)


#### Box-Plot of CIs #########
data_pmiss_eq_CI <- subset(data_pmiss_eq_CI, subset = value < 10)
p_mis_eq_CI <- ggplot(data_pmiss_eq_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Equal Sample Size") +
  xlab("")+
  ylab("95% Confidence Interval") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p_mis_eq_CI)


#################################### TREATMENT 3 TIMES CONTROL  ####################################
para_mis_T3C <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Parametric MisSpecified\\Para-MIS-S2-T3C.csv", header = TRUE)
para_mis_T3C <- transform(para_mis_T3C, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_mis_T3C_pe <- select(para_mis_T3C, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_mis_T3C_CI <- select(para_mis_T3C, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                          NNT_LP_DL, NNT_LP_BS, n)



data_pmiss_T3C_pe <- melt(para_mis_T3C_pe, id = c("n1","n0", "n")) 
data_pmiss_T3C_CI <- melt(para_mis_T3C_CI, id = c("n1","n0", "n"))
head(data_pmiss_T3C_pe)
head(data_pmiss_T3C_CI)


#### Box-Plot of Point Estimates #########
data_pmiss_T3C_pe <- subset(data_pmiss_T3C_pe, subset = value < 10)
pmis_T3C_pe <- ggplot(data_pmiss_T3C_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 3.22, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Treatment 3 times Control") + 
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pmis_T3C_pe)

#### Box-Plot of CIs #########
data_pmiss_T3C_CI <- subset(data_pmiss_T3C_CI, subset = value < 10)
pmis_T3C_CI <- ggplot(data_pmiss_T3C_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Treatment 3 times Control") +
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pmis_T3C_CI)


#################################### CONTROL 3 TIMES TREATMENT  ####################################
para_mis_C3T <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Parametric MisSpecified\\Para-MIS-S2-C3T.csv", header = TRUE)
para_mis_C3T <- transform(para_mis_C3T, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_mis_C3T_pe <- select(para_mis_C3T, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_mis_C3T_CI <- select(para_mis_C3T, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                          NNT_LP_DL, NNT_LP_BS, n)



data_pmiss_C3T_pe <- melt(para_mis_C3T_pe, id = c("n1","n0", "n")) 
data_pmiss_C3T_CI <- melt(para_mis_C3T_CI, id = c("n1","n0", "n"))
head(data_pmiss_C3T_pe)
head(data_pmiss_C3T_CI)


#### Box-Plot of Point Estimates #########
data_pmiss_C3T_pe <- subset(data_pmiss_C3T_pe, subset = value < 10)
pmis_C3T_pe <- ggplot(data_pmiss_C3T_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 3.22, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Control 3 times Treatment") + 
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pmis_C3T_pe)

#### Box-Plot of CIs #########
data_pmiss_C3T_CI <- subset(data_pmiss_C3T_CI, subset = value < 10)
pmis_C3T_CI <- ggplot(data_pmiss_C3T_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Control 3 times Treatment") +
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pmis_C3T_CI)


############################################ COMBINED BOX-PLOTS #######################################
############### POINT ESTIMATE ###############
mispara_pe <- ggarrange(pmis_eq_pe, pmis_T3C_pe, pmis_C3T_pe, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 3)  # number of rows
print(mispara_pe)
ggsave("F:/Output/Boxplots/Setup - 2/Para-Mis/ParaMIS-S2_PE.jpeg", height = 7 , width = 7 * 2)

############ CONFIDENCE INTERVAL ############
mispara_CI <- ggarrange(p_mis_eq_CI, pmis_T3C_CI, pmis_C3T_CI, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 3)  # number of rows
print(mispara_CI)
ggsave("F:/Output/Boxplots/Setup - 2/Para-Mis/ParaMIS-S2_CI.jpeg", height = 7 , width = 7 * 2)


####################################### SEMI - MISISPECIFIED #########################################
################## EQUAL SAMPLE SIZE ############
semi_mis_eq <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Semiparametric Misspecified\\Semi-MisSpec-S2-EQ.csv", header = TRUE)

semi_mis_eq <- transform(semi_mis_eq, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ### 
semi_mis_eq_pe <-  select(semi_mis_eq, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 

#### Dataset for CI ###
semi_mis_eq_CI <- select(semi_mis_eq, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                         NNT_LP_DL, NNT_LP_BS, n)

data_long_eq_pe <- melt(semi_mis_eq_pe, id = c("n1", "n0", "n")) 
data_long_eq_CI <- melt(semi_mis_eq_CI, id = c("n1","n0", "n"))


#### Box-Plot of Point Estimates #########
data_long_eq_pe <- subset(data_long_eq_pe, subset = value < 10)
psemi_mis2_eq_pe <- ggplot(data_long_eq_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 5.47, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Equal Sample Size") +
  xlab("")+
  ylab("NNT") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(psemi_mis2_eq_pe)

#### Box-Plot of CIs #########
data_long_eq_CI <- subset(data_long_eq_CI, subset = value < 10)
psemi_mis2_eq_CI <- ggplot(data_long_eq_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  xlab("")+
  ggtitle("Equal Sample Size")+
  ylab("95% Confidence Interval") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(psemi_mis2_eq_CI)


################################### TREATMENT 3 TIMES CONTROL  #######################################################
semi_mis_T3C <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Semiparametric Misspecified\\Semi-MisSpec-S2-T3C.csv", header = TRUE)

semi_mis_T3C <- transform(semi_mis_T3C, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ### 
semi_mis_T3C_pe <-  select(semi_mis_T3C, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 
head(semi_mis_T3C_pe)


#### Dataset for CI ###
semi_mis_T3C_CI <- select(semi_mis_T3C, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                         NNT_LP_DL, NNT_LP_BS, n)
head(semi_mis_T3C_CI)

data_long_T3C_pe <- melt(semi_mis_T3C_pe, id = c("n1", "n0", "n")) 
data_long_T3C_CI <- melt(semi_mis_T3C_CI, id = c("n1","n0", "n"))
head(data_long_T3C_pe)
head(data_long_T3C_CI)


#### Box-Plot of Point Estimates #########
data_long_T3C_pe <- subset(data_long_T3C_pe, subset = value < 10)
psemi_mis2_T3C_pe <- ggplot(data_long_T3C_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 5.47, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Treatment 3 times Control") +
  xlab("")+
  ylab(" ") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(psemi_mis2_T3C_pe)

#### Box-Plot of CIs #########
data_long_T3C_CI <- subset(data_long_T3C_CI, subset = value < 10)
psemi_mis2_T3C_CI <- ggplot(data_long_T3C_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  xlab("")+
  ggtitle("Treatment 3 times Control")+
  ylab("") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(psemi_mis2_T3C_CI)



################################### CONTROL 3 TIMES TREATMENT  #######################################################
semi_mis_C3T <- read.csv("F:\\Output\\CSV Files\\Setup - 2\\Semiparametric Misspecified\\Semi-MisSpec-S2-C3T.csv", header = TRUE)

semi_mis_C3T <- transform(semi_mis_C3T, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ### 
semi_mis_C3T_pe <-  select(semi_mis_C3T, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 
head(semi_mis_C3T_pe)


#### Dataset for CI ###
semi_mis_C3T_CI <- select(semi_mis_C3T, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                          NNT_LP_DL, NNT_LP_BS, n)
head(semi_mis_T3C_CI)

data_long_C3T_pe <- melt(semi_mis_C3T_pe, id = c("n1", "n0", "n")) 
data_long_C3T_CI <- melt(semi_mis_C3T_CI, id = c("n1","n0", "n"))
head(data_long_C3T_pe)
head(data_long_C3T_CI)


#### Box-Plot of Point Estimates #########
data_long_C3T_pe <- subset(data_long_C3T_pe, subset = value < 10)
psemi_mis2_C3T_pe <- ggplot(data_long_C3T_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 5.47, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Control 3 times Treatment") +
  xlab("")+
  ylab(" ") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(psemi_mis2_C3T_pe)

#### Box-Plot of CIs #########
data_long_C3T_CI <- subset(data_long_C3T_CI, subset = value < 10)
psemi_mis2_C3T_CI <- ggplot(data_long_C3T_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  xlab("")+
  ggtitle("Control 3 times Treatment")+
  ylab("") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(psemi_mis2_C3T_CI)


############## Box-Plot for misspecified semiparametric model #############


semi_pe <- ggarrange(psemi_mis2_eq_pe, psemi_mis2_T3C_pe, psemi_mis2_C3T_pe, # list of plots
                common.legend = T, # COMMON LEGEND
                legend = "bottom", # legend position
                align = "hv", # Align them both, horizontal and vertical
                ncol = 3)  # number of rows
print(semi_pe)
ggsave("F:/Output/Boxplots/Setup - 2/Sp-MIs/Semi-MIS-S2_PE.jpeg", height = 7 , width = 7 * 2)



semi_CI <- ggarrange(psemi_mis2_eq_CI, psemi_mis2_T3C_CI, psemi_mis2_C3T_CI, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 3)  # number of rows
print(semi_CI)
ggsave("F:/Output/Boxplots/Setup - 2/Sp-MIs/Semi-MIS-S2_CI.jpeg", height = 7 , width = 7 * 2)
