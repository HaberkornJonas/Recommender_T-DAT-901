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
projectPath <- ""
## projectPath <- "C:/Users/jonas/Desktop/T-DAT/"
## projectPath <- "E:/Projet/Epitech/t-dat-901/"
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

cl <- kmeans(S_R[,-1], 5, iter.max=20)                                                                             # Calculate kmeans based on mean number and standard deviation of tickets per month per customer 
plot <- fviz_cluster(cl,
                     S_R[,-1], 
                     geom="point",
                     main="Repartition des clients par leur regularite",
                     xlab="Moyenne de tickets par mois",
                     ylab="Ecart-type de tickets par mois")
#print(plot)
S_R <- cbind(S_R, ClusterId=cl$cluster)                                                                            # Bind ClientId and data to ClusterId                        

# Sauvegarde des donnees
write_csv(S_R, paste(projectPath, "03_output/S_R.csv", sep=""))

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/S_R.png", sep=""), width = 10, height = 8, dpi = 100)
rm(C_T_M_M)
rm(C_T_M_S)
rm(S_R)
rm(cl)
rm(plot)
sprintf("[S_R] Done!")




## [S_S_T] ############################### 
print("[S_S_T]...")

# Lecture des donnees
C_S_T_M <- read_csv(paste(projectPath, "03_output/C_S_T_M.csv", sep=""))
C_S_T_S <- read_csv(paste(projectPath, "03_output/C_S_T_S.csv", sep=""))

# Segmenting customers
S_S_T <- C_S_T_M %>%
  merge(C_S_T_S, by="ClientId", sort=F)  

cl <- kmeans(S_S_T[,-1], 8, iter.max=8)                                                                            # Calculate kmeans based on mean number and standard deviation of the price per tickets per month per customer 
plot <- fviz_cluster(cl,
                     S_S_T[,-1], 
                     geom="point",
                     main="Repartition des clients par leur depenses par ticket",
                     xlab="Moyenne des depenses par ticket",
                     ylab="Ecart-type des depenses par ticket")
#print(plot)
S_S_T <- cbind(S_S_T, ClusterId=cl$cluster)                                                                        # Bind ClientId and data to ClusterId                        

# Sauvegarde des donnees
write_csv(S_S_T, paste(projectPath, "03_output/S_S_T.csv", sep=""))

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/S_S_T.png", sep=""), width = 10, height = 8, dpi = 100)
rm(C_S_T_M)
rm(C_S_T_S)
rm(S_S_T)
rm(cl)
rm(plot)
sprintf("[S_S_T] Done!")




## [S_S_I] ############################### 
print("[S_S_I]...")

# Lecture des donnees
C_S_P_M <- read_csv(paste(projectPath, "03_output/C_S_P_M.csv", sep=""))
C_S_P_S <- read_csv(paste(projectPath, "03_output/C_S_P_S.csv", sep=""))

# Segmenting customers
S_S_I <- C_S_P_M %>%
  merge(C_S_P_S, by="ClientId", sort=F)  

cl <- kmeans(S_S_I[,-1], 8, iter.max=8)                                                                            # Calculate kmeans based on mean and standard deviation of price per product per customer 
plot <- fviz_cluster(cl,
                     S_S_I[,-1], 
                     geom="point",
                     main="Repartition des clients par leur depenses par produit",
                     xlab="Moyenne des depenses par produit",
                     ylab="Ecart-type des depenses par produit")
#print(plot)
S_S_I <- cbind(S_S_I, ClusterId=cl$cluster)                                                                        # Bind ClientId and data to ClusterId                        

# Sauvegarde des donnees
write_csv(S_S_I, paste(projectPath, "03_output/S_S_I.csv", sep=""))

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/S_S_I.png", sep=""), width = 10, height = 8, dpi = 100)
rm(C_S_P_M)
rm(C_S_P_S)
rm(S_S_I)
rm(cl)
rm(plot)
sprintf("[S_S_I] Done!")




## [S_F] ############################### 
print("[S_F]...")

# Lecture des donnees
C_F_Y_MF <- read_csv(paste(projectPath, "03_output/C_F_Y_MF.csv", sep=""))

# Segmenting customers
S_F <- C_F_Y_MF 

cl <- kmeans(S_F[,-c(1,2)], count(distinct(S_F, FamilleId))$n, iter.max=count(distinct(S_F, FamilleId))$n)         # Calculate kmeans based on mean number and standard deviation of tickets per month per customer 
plot <- fviz_cluster(cl,
                     S_F[,-1], 
                     geom="point",
                     main="Repartition des clients par leur famille de produits favorite",
                     xlab="Nombre de produit achetes",
                     ylab="ID de la famille de produit")
#print(plot)
S_F <- cbind(S_F, ClusterId=cl$cluster)                                                                            # Bind ClientId and data to ClusterId   

# Sauvegarde des donnees
write_csv(S_F, paste(projectPath, "03_output/S_F.csv", sep=""))

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/S_F.png", sep=""), width = 10, height = 8, dpi = 100)
rm(C_F_Y_MF)
rm(S_F)
rm(cl)
rm(plot)
sprintf("[S_F] Done!")
