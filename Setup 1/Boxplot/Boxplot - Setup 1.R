##################################################################################################################

#' @Description - This R program creates boxplots of Nonparametric, Parametric and Semiparametric
#                 estimator of Laupacis'NNT. In this program, boxplots of both point estimator and   
#                 CIs are constructed for Setup - I (Correctly Specified Parametric Model,
#                 Misspecified Parametric Model and Misspecified Semiparametric Model)for different
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


###################################### SETUP 1 ##############################################
####################### CORRECTLY SPECIFIED PARAMETRIC MODEL ################
#####################    EQUAL SAMPLE SIZE     #################################
para_eq <- read.csv("F:\\Output\\CSV Files\\Setup - 1\\Parametric Specified\\Para-SP-EQ-S1.csv", header = TRUE)
para_eq <- transform(para_eq, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_eq_pe <- select(para_eq, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_eq_CI <- select(para_eq, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                     NNT_LP_DL, NNT_LP_BS, n)

data_peq_pe <- melt(para_eq_pe, id = c("n1","n0", "n")) 
data_peq_CI <- melt(para_eq_CI, id = c("n1","n0", "n"))
head(data_peq_pe)
head(data_peq_CI)


#### Box-Plot of Point Estimates #########
data_peq_pe <- subset(data_peq_pe, subset = value < 10)
peq_pe <- ggplot(data_peq_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 2.93, linetype = "solid", col = "red", linewidth = 0.75)     +
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
print(peq_pe)

#### Box-Plot of CIs #########
data_peq_CI <- subset(data_peq_CI, subset = value < 10)
peq_CI <-ggplot(data_peq_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
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
print(peq_CI)

#######################  TREATMENT ARM HAVE SAMPLE SIZE 3 TIMES CONTROL ARM  #################################
para_T3C <- read.csv("F:\\Output\\CSV Files\\Setup - 1\\Parametric Specified\\Para-SP-T3C-S1.csv", header = TRUE)
para_T3C <- transform(para_T3C, n=(interaction(n1, n0, sep =":")))
head(para_T3C)

### Dataset for Point Estimate ###  
para_T3C_pe <- select(para_T3C, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_T3C_CI <- select(para_T3C, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                      NNT_LP_DL, NNT_LP_BS, n)


data_pT3C_pe <- melt(para_T3C_pe, id = c("n1","n0", "n")) 
data_pT3C_CI <- melt(para_T3C_CI, id = c("n1","n0", "n"))
head(data_pT3C_pe)
head(data_pT3C_CI)


#### Box-Plot of Point Estimates #########
data_pT3C_pe <- subset(data_pT3C_pe, subset = value < 10)
pT3C_pe <- ggplot(data_pT3C_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 2.93, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Treatment 3 times Control")+
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(2, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pT3C_pe)

#### Box-Plot of CIs #########
data_pT3C_CI <- subset(data_pT3C_CI, subset = value < 10)
pT3C_CI <- ggplot(data_pT3C_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Treatment 3 times Control")+
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pT3C_CI)

############################################ COMBINED BOX-PLOTS #######################################
############### POINT ESTIMATE ###############
para_S1_pe <- ggarrange(peq_pe, pT3C_pe, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 2)  # number of rows
print(para_S1_pe)
ggsave("F:/Output/Boxplots/Setup - 1/Para Specified/Para-S1_PE.jpeg", height = 7 , width = 7 * 2)

############ CONFIDENCE INTERVAL ############
para_S1_CI <- ggarrange(peq_CI, pT3C_CI, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 2)  # number of rows
print(para_S1_CI)
ggsave("F:/Output/Boxplots/Setup - 1/Para Specified/Para-S1_CI.jpeg", height = 7 , width = 7 * 2)



############################### MISSPECIFIED PARAMETRIC MODEL ##################################
######################### EQUAL SAMPLE SIZE ##############################
para_mis_eq <- read.csv("F:\\Output\\CSV Files\\Setup - 1\\Misspecified Parametric\\Para-MIS-EQ-S1.csv", header = TRUE)
para_mis_eq <- transform(para_mis_eq, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_mis_eq_pe <- select(para_mis_eq, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 


### Dataset for CI ###
para_mis_eq_CI <- select(para_mis_eq, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                         NNT_LP_DL, NNT_LP_BS, n)


data_pmis_eq_pe <- melt(para_mis_eq_pe, id = c("n1","n0", "n")) 
data_pmis_eq_CI <- melt(para_mis_eq_CI, id = c("n1","n0", "n"))
head(data_pmis_eq_pe)
head(para_mis_eq_CI)


#### Box-Plot of Point Estimates #########
data_pmis_eq_pe <- subset(data_pmis_eq_pe, subset = value < 10)
pmis_eq_pe <- ggplot(data_pmis_eq_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 2.58, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Equal Sample Size") +
  xlab("")+
  ylab("NNT") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pmis_eq_pe)

#### Box-Plot of CIs #########
data_pmis_eq_CI <- subset(data_pmis_eq_CI, subset = value < 10)
pmis_eq_CI <- ggplot(data_pmis_eq_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
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
print(pmis_eq_CI)

#################################### TREATMENT 3 TIMES CONTROL  ####################################
para_mis_T3C <- read.csv("F:\\Output\\CSV Files\\Setup - 1\\Misspecified Parametric\\Para-MIS-T3C-S1.csv", header = TRUE)
para_mis_T3C <- transform(para_mis_T3C, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ###  
para_mis_T3C_pe <- select(para_mis_T3C, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 

### Dataset for CI ###
para_mis_T3C_CI <- select(para_mis_T3C, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                          NNT_LP_DL, NNT_LP_BS, n)


data_pmis_T3C_pe <- melt(para_mis_T3C_pe, id = c("n1","n0", "n")) 
data_pmis_T3C_CI <- melt(para_mis_T3C_CI, id = c("n1","n0", "n"))
head(data_pmis_T3C_pe)
head(data_pmis_T3C_CI)


#### Box-Plot of Point Estimates #########
data_pmis_T3C_pe <- subset(data_pmis_T3C_pe, subset = value < 10)
pmis_T3C_pe <- ggplot(data_pmis_T3C_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 2.58, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Treatment 3 times Control")+
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(pmis_T3C_pe)

#### Box-Plot of CIs #########
data_pmis_T3C_CI <- subset(data_pmis_T3C_CI, subset = value < 10)
pmis_T3C_CI <- ggplot(data_pmis_T3C_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Treatment 3 times Control")+
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


############################################ COMBINED BOX-PLOTS #######################################
############### POINT ESTIMATE ###############
para_mis_pe <- ggarrange(pmis_eq_pe, pmis_T3C_pe, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 2)  # number of rows
print(para_mis_pe)
ggsave("F:/Output/Boxplots/Setup - 1/Para - Mis/Para-S1_MIS_PE.jpeg", height = 7 , width = 7 * 2)

############ CONFIDENCE INTERVAL ############
para_mis_CI <- ggarrange(pmis_eq_CI, pmis_T3C_CI, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 2)  # number of rows
print(para_mis_CI)
ggsave("F:/Output/Boxplots/Setup - 1/Para - Mis/Para-S1_MIS_CI.jpeg", height = 7 , width = 7 * 2)


############################### MISSPECIFIED SEMIPARAMETRIC MODEL ##################################
######################### EQUAL SAMPLE SIZE ##############################
sp_mis_eq <- read.csv("F:\\Output\\CSV Files\\Setup - 1\\Semi - mis\\Semi-MIS-EQ-S1.csv", header = TRUE)
sp_mis_eq <- transform(sp_mis_eq, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ### 
sp_mis_eq_pe <-  select(sp_mis_eq, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 

#### Dataset for CI ###
sp_mis_eq_CI <- select(sp_mis_eq, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                         NNT_LP_DL, NNT_LP_BS, n)



data_spmis_eq_pe <- melt(sp_mis_eq_pe, id = c("n1","n0", "n")) 
data_spmis_eq_CI <- melt(sp_mis_eq_CI, id = c("n1","n0", "n"))
head(data_spmis_eq_pe)
head(data_spmis_eq_CI)



#### Box-Plot of Point Estimates #########
data_spmis_eq_pe <- subset(data_spmis_eq_pe, subset = value < 10)
sp_mis_eq_pe <- ggplot(data_spmis_eq_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 4.53, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Equal Sample Size") +
  xlab("")+
  ylab("NNT") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
   theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
print(sp_mis_eq_pe)

#### Box-Plot of CIs #########
data_spmis_eq_CI <- subset(data_spmis_eq_CI, subset = value < 10)
sp_mis_eq_CI <- ggplot(data_spmis_eq_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Equal Sample Size") +
  xlab(" ")+
  ylab("95% Confidence Interval") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(sp_mis_eq_CI)


###################################  Treatment arm 3 times control arm #################################
sp_mis_T3C <- read.csv("F:\\Output\\CSV Files\\Setup - 1\\Semi - mis\\Semi-MIS-T3C-S1.csv", header = TRUE)
sp_mis_T3C <- transform(sp_mis_T3C, n=(interaction(n1, n0, sep =":")))

### Dataset for Point Estimate ### 
sp_mis_T3C_pe <-  select(sp_mis_T3C, n1, n0, NNT_DR , NNT_L, NNT_LP, n) 

#### Dataset for CI ###
sp_mis_T3C_CI <- select(sp_mis_T3C, n1, n0, NNT_DR_DL, NNT_DR_BS, NNT_L_WL, NNT_L_DL, NNT_L_BS,
                       NNT_LP_DL, NNT_LP_BS, n)



data_spmis_T3C_pe <- melt(sp_mis_T3C_pe, id = c("n1","n0", "n")) 
data_spmis_T3C_CI <- melt(sp_mis_T3C_CI, id = c("n1","n0", "n"))
head(data_spmis_T3C_pe)
head(data_spmis_T3C_CI)



#### Box-Plot of Point Estimates #########
data_spmis_T3C_pe <- subset(data_spmis_T3C_pe, subset = value < 10)
sp_mis_T3C_pe <- ggplot(data_spmis_T3C_pe, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  geom_hline(yintercept = 4.53, linetype = "solid", col = "red", linewidth = 0.75)     +
  ggtitle("Treatment 3 times Control") +
  xlab("")+
  ylab("") +
  scale_fill_discrete(name = "Estimators")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(sp_mis_T3C_pe)

#### Box-Plot of CIs #########
data_spmis_T3C_CI <- subset(data_spmis_T3C_CI, subset = value < 10)
sp_mis_T3C_CI <- ggplot(data_spmis_T3C_CI, aes(x = factor(n), y = value, fill= variable)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Treatment 3 times Control") +
  xlab(" ")+
  ylab("") +
  scale_fill_discrete(name = "CI Type")+
  scale_y_continuous(breaks = 1:15)  +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "bottom") +
  theme(plot.title  = element_text(hjust = 0.5,     size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(sp_mis_T3C_CI)


############################################ COMBINED BOX-PLOTS #######################################
############### POINT ESTIMATE ###############
semi_mis_pe <- ggarrange(sp_mis_eq_pe, sp_mis_T3C_pe, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 2)  # number of rows
print(semi_mis_pe)
ggsave("F:/Output/Boxplots/Setup - 1/SemiPara - MIS/Semi_MIS-S1_PE.jpeg", height = 7 , width = 7 * 2)

############ CONFIDENCE INTERVAL ############
semi_mis_CI <- ggarrange(sp_mis_eq_CI, sp_mis_T3C_CI, # list of plots
                     common.legend = T, # COMMON LEGEND
                     legend = "bottom", # legend position
                     align = "hv", # Align them both, horizontal and vertical
                     ncol = 2)  # number of rows
print(semi_mis_CI)
ggsave("F:/Output/Boxplots/Setup - 1/SemiPara - MIS/Semi_MIS-S1_CI.jpeg", height = 7 , width = 7 * 2)

