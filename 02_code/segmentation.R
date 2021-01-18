## IMPORTS ###############################
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(Hmisc)
library(tidyr)

library(cluster)
library(factoextra)
library(magrittr)
library(FactoMineR)

## STATIC VALUES ###############################
projectPath <- "C:/Users/jonas/Desktop/T-DAT/"
moisIds <- c(1:12)
moisNoms <- c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
moisDictionary <- data.frame(moisVenteId=c(1:12), moisVente=moisNoms)

Customers <- read_csv(paste(projectPath, "03_output/Customers.csv", sep=""))


####  [S] Segmentation ####
## [S_R] ############################### 
print("[S_R]...")

# Lecture des donnees
C_T_M_M <- read_csv(paste(projectPath, "03_output/C_T_M_M.csv", sep=""))
C_T_M_S <- read_csv(paste(projectPath, "03_output/C_T_M_S.csv", sep=""))

# Segmenting customers
S_R <- C_T_M_M %>%
  merge(C_T_M_S, by="ClientId", sort=F)  

cl <- kmeans(S_R[,-1], 5, iter.max=20)                                         # Calculate kmeans based on mean number and standard deviation of tickets per month per customer 
plot <- fviz_cluster(cl,
                     S_R[,-1], 
                     geom="point",
                     main="Répartition des clients par leur régularité")
print(plot)
S_R <- cbind(S_R, ClusterId=cl$cluster)                                        # Bind ClientId and data to ClusterId                        

# Sauvegarde des donnees
write_csv(S_R, paste(projectPath, "03_output/S.R.csv", sep=""))

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/S_R.png", sep=""), width = 10, height = 8, dpi = 100)
sprintf("[S_R] Done!")




## [S_S_T] ############################### 
# TODO




## [S_S_I] ############################### 
# TODO




## [S_F] ############################### 
# TODO




## [S_G] ############################### 
# TODO (Bonus)
