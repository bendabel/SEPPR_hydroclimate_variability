###PCA
library(tidyverse)

###Enter name of desired file for PCA
PPR_precip_JJAS_avg_CD <- read_csv("./NCEI_CAG_Data/Climate_Divisions/Precip/PPR_CD_precip_JJAS_avg.csv")
#PPR_precip_MAM_avg <- read_csv("./NCEI_CAG_Data/Climate_Divisions/Precip/PPR_precip_MAM_avg.csv")

#Exclude row with years
PCA_file <- PPR_precip_JJAS_avg_CD[ ,2:23]

#Scale data
PCA_data_scaled <- scale(PCA_file)

#####
###Calculate using SVD
#####
# #Get variance matrix
# Zs = var(PCA_data_scaled)
# 
# #Eigen decomposition
# Zsvd = svd(Zs)
# 
# #Principal component scores
# PCs = t(t(Zsvd$u) %*% t(PCA_data_scaled))
# 
# #Eigen values - fraction variance 
# lambdas = (Zsvd$d/sum(Zsvd$d))
# 
# #write pcs to file
# PC_values <- as_tibble(PCs)
# #write_csv(PC_values, "./PC_values_PPR_precip_JJAS.csv")

#####
#Use built in function
PCA_2 <- princomp(PCA_data_scaled, cor = F)

#loadings/eigenvectors
eig_vec <- PCA_2$loadings

#pc scores
PC_scores <- PCA_2$scores

#eigenvalues
eig_val <- (PCA_2$sdev)^2

#fraction variance 
frac_var <- ( eig_val/sum(eig_val) )
