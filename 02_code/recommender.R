## IMPORTS ###############################
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(Hmisc)
library(tidyr)
library(gridExtra)
library(stringr)


## STATIC VALUES ###############################
projectPath <- ""
## projectPath <- "C:/Users/jonas/Desktop/T-DAT/"
## projectPath <- "E:/Projet/Epitech/t-dat-901/"
moisIds <- c(1:12)
moisNoms <- c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
moisDictionary <- data.frame(moisVenteId=c(1:12), moisVente=moisNoms)


Customers <- read_csv(paste(projectPath, "03_output/Customers.csv", sep=""))
Families <- read_csv(paste(projectPath, "03_output/Familles.csv", sep=""))
Products <- read_csv(paste(projectPath, "03_output/Produits.csv", sep=""))

options("scipen"=100)


## GETTING CLIENT ID ###############################
repeat{
  clientId <- as.numeric(readline(prompt="Enter a ClientId: "))
  if (!is.na(clientId)){
    if(clientId %in% Customers$ClientId){
      break
    }
    else{
      print("L'ID du client renseignee n'a pas ete trouvee, veuillez reessayer.")
    }
  }
  else{
    print("L'ID du client renseignee n'est pas valide, veuillez reessayer.")
  }
}


## SETUP CUSTOMER DIRECTORY ###############################
clientDir <- paste(projectPath, "03_output/Client_", clientId, sep="")
unlink(clientDir, recursive = TRUE)
dir.create(file.path(clientDir), showWarnings = FALSE)
descriptionFile<-paste(projectPath, "03_output/Client_", clientId, "/description.txt", sep="")
clientProfile <- data.frame(ClientId=clientId)


## CLIENT STATISTICAL DESCRIPTION ###############################
print("## Client statistical dedscripton")
write("----- DESCRIPTION STATISTIQUE DU CLIENT -----", file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

# Client ID
write(paste("ClientId:",clientId), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

# General statistics
## Loading data ##
C_T_Y_T <- read_csv(paste(projectPath, "03_output/C_T_Y_T.csv", sep=""))
G_T_Y_T <- read_csv(paste(projectPath, "03_output/G_T_Y_T.csv", sep=""))
C_P_Y_T <- read_csv(paste(projectPath, "03_output/C_P_Y_T.csv", sep=""))
G_C_P_Y_M <- read_csv(paste(projectPath, "03_output/G_C_P_Y_M.csv", sep=""))
C_S_Y_T <- read_csv(paste(projectPath, "03_output/C_S_Y_T.csv", sep=""))
C_P_M_M <- read_csv(paste(projectPath, "03_output/C_P_M_M.csv", sep=""))
C_P_M_T <- read_csv(paste(projectPath, "03_output/C_P_M_T.csv", sep=""))
C_T_M_T <- read_csv(paste(projectPath, "03_output/C_T_M_T.csv", sep=""))
C_S_M_T <- read_csv(paste(projectPath, "03_output/C_S_M_T.csv", sep=""))
G_S_Y_T <- read_csv(paste(projectPath, "03_output/G_S_Y_T.csv", sep=""))
C_F_Y_MF <- read_csv(paste(projectPath, "03_output/C_F_Y_MF.csv", sep=""))
C_F_Y_T <- read_csv(paste(projectPath, "03_output/C_F_Y_T.csv", sep=""))
C_F_MF_Y <- read_csv(paste(projectPath, "03_output/C_F_MF_Y.csv", sep=""))
C_MF_Y <- read_csv(paste(projectPath, "03_output/C_MF_Y.csv", sep=""))

## Writing data in the file ## 
### Number of tickets
write(paste("Nombre total de tickets: ", C_T_Y_T[C_T_Y_T$ClientId==clientId,]$n, " (", signif((C_T_Y_T[C_T_Y_T$ClientId==clientId,]$n*100)/G_T_Y_T$n, 3), "% des tickets du magasin)", sep=""), file=descriptionFile, append=T)

### Number of products bought over the year
write(paste("Nombre total de produits achetes: ", C_P_Y_T[C_P_Y_T$ClientId==clientId,]$n, " (la moyenne generale de produits achete par client etant de ", round(G_C_P_Y_M$mean, digits=2), ")", sep=""), file=descriptionFile, append=T)

### Mean number of products bought per month 
write(paste("Nombre moyen de produits achetes par mois: ", C_P_M_M[C_P_M_M$ClientId==clientId,]$mean, sep=""), file=descriptionFile, append=T)

### Total spendings over the year
write(paste("Depenses totales sur l'annee: ", C_S_Y_T[C_S_Y_T$ClientId==clientId, ]$n," euros (", signif((C_S_Y_T[C_S_Y_T$ClientId==clientId,]$n*100/G_S_Y_T),3), "% des revenus du magasin)", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

### Number of vistis, products bought and spendings per month
write("Statistiques par mois: ", file=descriptionFile, append=T)
mostPurchaseMonth <- 0
for(month in moisIds){
  customerPurchases <- C_P_M_T[C_P_M_T$ClientId==clientId, ]
  productBoughtDuringCurrentMonth <- customerPurchases[customerPurchases$MoisVenteId==month, ]$n
  
  customerSpendings <- C_S_M_T[C_S_M_T$ClientId==clientId, ]
  currentMonthCustomerSpending <- customerSpendings[customerSpendings$MoisVenteId==month, ]$n
  
  visitByMonth <- C_T_M_T[C_T_M_T$ClientId==clientId, ]
  currentMonthVisit <- visitByMonth[visitByMonth$MoisVenteId==month, ]$n
  
  if(length(productBoughtDuringCurrentMonth) == 0){
    productBoughtDuringCurrentMonth <- 0
  }
  
  if(length(currentMonthCustomerSpending) == 0){
    currentMonthCustomerSpending <- 0
  }
  
  if(length(currentMonthVisit) == 0){
    currentMonthVisit <- 0
  }
  
  write(paste("   ", str_pad(moisNoms[month],9,'right',' '),": ", productBoughtDuringCurrentMonth, " achats, ", currentMonthVisit, " visites, ", currentMonthCustomerSpending, " euros", sep=""), file=descriptionFile, append=T)
}
write(paste("", sep=""), file=descriptionFile, append=T)

### Favorite product families
yearlyMostPreferedFamilies <- C_F_Y_MF %>% 
  filter(C_F_Y_MF$ClientId==clientProfile$ClientId) %>%
  merge(Families, by='FamilleId', sort=F)
if(count(yearlyMostPreferedFamilies)$n > 0){
  write(paste("Famille de produits prefere: ", yearlyMostPreferedFamilies$Famille," (avec ", yearlyMostPreferedFamilies$n," produits achetes)", sep=""), file=descriptionFile, append=T)
}
write(paste("", sep=""), file=descriptionFile, append=T)


### Number of products bought and favorite products per product families
write("Statistiques par famille de produits: ", file=descriptionFile, append=T)
client_C_F_Y_T <- C_F_Y_T %>% 
  filter(C_F_Y_T$ClientId==clientProfile$ClientId)
client_C_F_MF_Y <- C_F_MF_Y %>% 
  filter(C_F_MF_Y$ClientId==clientProfile$ClientId) %>%
  merge(Products, by='ProduitId', sort=F)
if(count(Families)$n > 0){
  for(family_index in 1:count(Families)$n){
    family_data <- client_C_F_Y_T %>% filter(client_C_F_Y_T$FamilleId==Families[family_index,]$FamilleId)
    if(count(family_data)$n > 0){
      write(paste("   - ", Families[family_index,]$Famille," (", family_data$n ," produits achetes)", sep=""), file=descriptionFile, append=T)
    } else {
      write(paste("   - ", Families[family_index,]$Famille," (",       0       ," produits achetes)", sep=""), file=descriptionFile, append=T)
    }
    
    preferedProduct <- client_C_F_MF_Y %>% 
      filter(client_C_F_MF_Y$FamilleId==Families[family_index,]$FamilleId) %>%
      arrange(desc(n))
    if(count(preferedProduct)$n > 0){
      write(paste("       Produits preferes: ",sep=""),file=descriptionFile, append=T)
      for(product_index in 1:count(preferedProduct)$n){
        write(paste("           - ", preferedProduct[product_index,]$Libelle," (achete ", preferedProduct[product_index,]$n," fois)", sep=""), file=descriptionFile, append=T)
      }
    }
  }
}
write(paste("", sep=""), file=descriptionFile, append=T)


### Most favorite products over the year
write("Produits preferes: ", file=descriptionFile, append=T)
yearlyMostPreferedProducts <- C_MF_Y %>% 
  filter(C_MF_Y$ClientId==clientProfile$ClientId) %>%
  merge(Products %>% select(ProduitId, Libelle), by='ProduitId', sort=F) %>%
  arrange(desc(n))
if(count(yearlyMostPreferedProducts)$n > 0){
  for(product_index in 1:count(yearlyMostPreferedProducts)$n){
    write(paste("   - ", yearlyMostPreferedProducts[product_index,]$Libelle," [", yearlyMostPreferedProducts[product_index,]$Famille,"] (achete ", yearlyMostPreferedProducts[product_index,]$n, " fois)", sep=""), file=descriptionFile, append=T)
  }
}
write(paste("", sep=""), file=descriptionFile, append=T)


## CLIENT SEGMENTATION ANALYSIS ###############################
print("## Client segmentation analysis")
write(paste("", sep=""), file=descriptionFile, append=T)
write("----- ANALYSE DE LA SEGMENTATION -----", file=descriptionFile, append=T)



## [S_R] Regularity
write(paste("", sep=""), file=descriptionFile, append=T)
S_R <- read_csv(paste(projectPath, "03_output/S_R.csv", sep=""))  # Getting segmentation result

# Extracting clusterId and data
S_R.client <- subset(S_R, ClientId == clientId)                                # Extracting client entry
mean <- S_R.client$mean                                                        # Getting client mean value
sd <- S_R.client$sd                                                            # Getting client sd

S_R.client <- S_R.client %>%                                                   
  select(ClientId, ClusterId) %>%                                              # Keeping only ClientId and ClusterId
  rename(S_R = ClusterId)                                                      # Renaming CluesterId column name to prepare merging

clientProfile <- clientProfile %>%
  merge(S_R.client, by="ClientId", sort=F)                                     # Merging ClusterId into the client profile

# Ordering groups
orderedGoups <- S_R %>%
  group_by(ClusterId) %>%                                                      # Grouping segmentation data by ClusterId
  summarise(meanDist = mean(sqrt(mean*mean+sd*sd))) %>%                        # Calculating mean distance with origin for each cluster
  arrange(desc(meanDist))                                                      # Ordering clusters by their distance

# Getting group position
groupPos <- match(clientProfile$S_R, orderedGoups$ClusterId)                   # Getting user's cluster position
group <- S_R %>%
  filter(S_R$ClusterId==clientProfile$S_R)                                     # Keeping only data from the same cluster as the user's cluster

# Getting client position in group and distance
orderedGroup <- S_R %>%
  filter(S_R$ClusterId==clientProfile$S_R) %>%
  transform(dist=round(sqrt(mean*mean+sd*sd), digits=2)) %>%
  arrange(desc(dist))
dist <- orderedGroup %>%
  filter(orderedGroup$ClientId==clientProfile$ClientId) %>%
  select(dist)
pos <- match(dist, orderedGroup$dist)

# Getting group data
maxMean  <-       max (group$mean)
minMean  <-       min (group$mean)
meanMean <- round(mean(group$mean)       , digits=2)
sdMean   <- round(sd  (group$mean)       , digits=2)
maxSD    <-       max (group$sd)
minSD    <-       min (group$sd)
meanSD   <- round(mean(group$sd)         , digits=2)
sdSD     <- round(sd  (group$sd)         , digits=2)
maxDist  <-       max (orderedGroup$dist)
minDist  <-       min (orderedGroup$dist)
meanDist <- round(mean(orderedGroup$dist), digits=2)
sdDist   <- round(sd  (orderedGroup$dist), digits=2)


# Writing segmentation analysis
write("- Regularite (vient souvent ou non)", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe en position ", groupPos, " sur ", count(orderedGoups)$n, " avec une regularite moyenne de ", mean, " tickets par mois et une deviation standard de ", sd, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe possede une moyenne maximum de ", maxMean, " et minimum de ", minMean, " tickets par mois pour une deviation standard maximum de ", maxSD, " et minimum de ", minSD, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec une distance a l'origine de ", dist, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_R, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Moyenne:              [", minMean, ", ", mean, ", ", maxMean, "] (mean: ", meanMean, ", sd: ", sdMean, ")", sep=""), file=descriptionFile, append=T)
write(paste("    Deviation standard:   [", minSD,   ", ", sd,   ", ", maxSD,   "] (mean: ", meanSD,   ", sd: ", sdSD,   ")", sep=""), file=descriptionFile, append=T)
write(paste("    Distance a l'origine: [", minDist, ", ", dist, ", ", maxDist, "] (mean: ", meanDist, ", sd: ", sdDist, ")", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)


# Generating plot
# Scatter plot of clients in the group
scatterPlot <- ggplot(group,aes(mean, sd)) + 
  geom_point() + 
  geom_point(data=group%>%filter(ClientId==clientProfile$ClientId), aes(mean, sd), color="red", size=3) +
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

# Marginal density curve for mean values
xdensity <- ggplot(group, aes(mean, fill=c('#999999'))) + 
  geom_density(alpha=.5, adjust=2) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")

# Marginal density curve for sd values
ydensity <- ggplot(group, aes(sd, fill=c('#999999'))) + 
  geom_density(alpha=.5, adjust=2) + 
  coord_flip() +
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")

# Create blank plot to hold previous plots
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  labs(
    title = paste("Position du client dans le groupe #", S_R.client$S_R, "\nDe la segementation S_R", sep=""),
    subtitle = "Donnees generees a partir du dataset KaDo"
  )

# Create and save grid plot
png(file=paste(clientDir, "/S_R.png", sep=""),
    width=1200, height=700)
gridPlot <- grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
dev.off()






# [S_S_T] Spendings per tickets
S_S_T <- read_csv(paste(projectPath, "03_output/S_S_T.csv", sep=""))  # Getting segmentation result

# Extracting clusterId and data
S_S_T.client <- subset(S_S_T, ClientId == clientId)                            # Extracting client entry
mean <- S_S_T.client$mean                                                      # Getting client mean value
sd <- S_S_T.client$sd                                                          # Getting client sd

S_S_T.client <- S_S_T.client %>%                                                   
  select(ClientId, ClusterId) %>%                                              # Keeping only ClientId and ClusterId
  rename(S_S_T = ClusterId)                                                    # Renaming CluesterId column name to prepare merging

clientProfile <- clientProfile %>%
  merge(S_S_T.client, by="ClientId", sort=F)                                   # Merging ClusterId into the client profile

# Ordering groups
orderedGoups <- S_S_T %>%
  group_by(ClusterId) %>%                                                      # Grouping segmentation data by ClusterId
  summarise(meanDist = mean(sqrt(mean*mean+sd*sd))) %>%                        # Calculating mean distance with origin for each cluster
  arrange(desc(meanDist))                                                      # Ordering clusters by their distance

# Getting group position
groupPos <- match(clientProfile$S_S_T, orderedGoups$ClusterId)                 # Getting user's cluster position
group <- S_S_T %>%
  filter(S_S_T$ClusterId==clientProfile$S_S_T)                                 # Keeping only data from the same cluster as the user's cluster

# Getting client position in group and distance
orderedGroup <- S_S_T %>%
  filter(S_S_T$ClusterId==clientProfile$S_S_T) %>%
  transform(dist=round(sqrt(mean*mean+sd*sd), digits=2)) %>%
  arrange(desc(dist))
dist <- orderedGroup %>%
  filter(orderedGroup$ClientId==clientProfile$ClientId) %>%
  select(dist)
pos <- match(dist, orderedGroup$dist)

# Getting group data
maxMean  <-       max (group$mean)
minMean  <-       min (group$mean)
meanMean <- round(mean(group$mean)       , digits=2)
sdMean   <- round(sd  (group$mean)       , digits=2)
maxSD    <-       max (group$sd)
minSD    <-       min (group$sd)
meanSD   <- round(mean(group$sd)         , digits=2)
sdSD     <- round(sd  (group$sd)         , digits=2)
maxDist  <-       max (orderedGroup$dist)
minDist  <-       min (orderedGroup$dist)
meanDist <- round(mean(orderedGroup$dist), digits=2)
sdDist   <- round(sd  (orderedGroup$dist), digits=2)


# Writing segmentation analysis
write("- Depenses par ticket (depenses beaucoup ou non)", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe en position ", groupPos, " sur ", count(orderedGoups)$n, " avec une depense par ticket moyenne de ", mean, " et une deviation standard de ", sd, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe possede une moyenne de depenses par ticket maximum de ", maxMean, " et minimum de ", minMean, " pour une deviation standard maximum de ", maxSD, " et minimum de ", minSD, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec une distance a l'origine de ", dist, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_S_T, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Moyenne:              [", minMean, ", ", mean, ", ", maxMean, "] (mean: ", meanMean, ", sd: ", sdMean, ")", sep=""), file=descriptionFile, append=T)
write(paste("    Deviation standard:   [", minSD,   ", ", sd,   ", ", maxSD,   "] (mean: ", meanSD,   ", sd: ", sdSD,   ")", sep=""), file=descriptionFile, append=T)
write(paste("    Distance a l'origine: [", minDist, ", ", dist, ", ", maxDist, "] (mean: ", meanDist, ", sd: ", sdDist, ")", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)


# Generating plot
# Scatter plot of clients in the group
scatterPlot <- ggplot(group,aes(mean, sd)) + 
  geom_point() + 
  geom_point(data=group%>%filter(ClientId==clientProfile$ClientId), aes(mean, sd), color="red", size=3) +
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

# Marginal density curve for mean values
xdensity <- ggplot(group, aes(mean, fill=c('#999999'))) + 
  geom_density(alpha=.5, adjust=2) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")

# Marginal density curve for sd values
ydensity <- ggplot(group, aes(sd, fill=c('#999999'))) + 
  geom_density(alpha=.5, adjust=2) + 
  coord_flip() +
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")

# Create blank plot to hold previous plots
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  labs(
    title = paste("Position du client dans le groupe #", S_S_T.client$S_S_T, "\nDe la segementation S_S_T", sep=""),
    subtitle = "Donnees generees a partir du dataset KaDo"
  )

# Create and save grid plot
png(file=paste(clientDir, "/S_S_T.png", sep=""),
    width=1200, height=700)
gridPlot <- grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
                         ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
dev.off()




# [S_S_I] Spendings per items
S_S_I <- read_csv(paste(projectPath, "03_output/S_S_I.csv", sep=""))  # Getting segmentation result

# Extracting clusterId and data
S_S_I.client <- subset(S_S_I, ClientId == clientId)                            # Extracting client entry
mean <- S_S_I.client$mean                                                      # Getting client mean value
sd <- S_S_I.client$sd                                                          # Getting client sd

S_S_I.client <- S_S_I.client %>%                                                   
  select(ClientId, ClusterId) %>%                                              # Keeping only ClientId and ClusterId
  rename(S_S_I = ClusterId)                                                    # Renaming CluesterId column name to prepare merging

clientProfile <- clientProfile %>%
  merge(S_S_I.client, by="ClientId", sort=F)                                   # Merging ClusterId into the client profile

# Ordering groups
orderedGoups <- S_S_I %>%
  group_by(ClusterId) %>%                                                      # Grouping segmentation data by ClusterId
  summarise(meanDist = mean(sqrt(mean*mean+sd*sd))) %>%                        # Calculating mean distance with origin for each cluster
  arrange(desc(meanDist))                                                      # Ordering clusters by their distance

# Getting group position
groupPos <- match(clientProfile$S_S_I, orderedGoups$ClusterId)                 # Getting user's cluster position
group <- S_S_I %>%
  filter(S_S_I$ClusterId==clientProfile$S_S_I)                                 # Keeping only data from the same cluster as the user's cluster

# Getting client position in group and distance
orderedGroup <- S_S_I %>%
  filter(S_S_I$ClusterId==clientProfile$S_S_I) %>%
  transform(dist=round(sqrt(mean*mean+sd*sd), digits=2)) %>%
  arrange(desc(dist))
dist <- orderedGroup %>%
  filter(orderedGroup$ClientId==clientProfile$ClientId) %>%
  select(dist)
pos <- match(dist, orderedGroup$dist)

# Getting group data
maxMean  <-       max (group$mean)
minMean  <-       min (group$mean)
meanMean <- round(mean(group$mean)       , digits=2)
sdMean   <- round(sd  (group$mean)       , digits=2)
maxSD    <-       max (group$sd)
minSD    <-       min (group$sd)
meanSD   <- round(mean(group$sd)         , digits=2)
sdSD     <- round(sd  (group$sd)         , digits=2)
maxDist  <-       max (orderedGroup$dist)
minDist  <-       min (orderedGroup$dist)
meanDist <- round(mean(orderedGroup$dist), digits=2)
sdDist   <- round(sd  (orderedGroup$dist), digits=2)

# Writing segmentation analysis
write("- Depenses par produit (achete des produits chere ou non)", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe en position ", groupPos, " sur ", count(orderedGoups)$n, " avec une depense par produit moyenne de ", mean, " et une deviation standard de ", sd, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe possede une moyenne de depenses par produit maximum de ", maxMean, " et minimum de ", minMean, " pour une deviation standard maximum de ", maxSD, " et minimum de ", minSD, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec une distance a l'origine de ", dist, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_S_I, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Moyenne:              [", minMean, ", ", mean, ", ", maxMean, "] (mean: ", meanMean, ", sd: ", sdMean, ")", sep=""), file=descriptionFile, append=T)
write(paste("    Deviation standard:   [", minSD,   ", ", sd,   ", ", maxSD,   "] (mean: ", meanSD,   ", sd: ", sdSD,   ")", sep=""), file=descriptionFile, append=T)
write(paste("    Distance a l'origine: [", minDist, ", ", dist, ", ", maxDist, "] (mean: ", meanDist, ", sd: ", sdDist, ")", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)


# Generating plot
# Scatter plot of clients in the group
scatterPlot <- ggplot(group,aes(mean, sd)) + 
  geom_point() + 
  geom_point(data=group%>%filter(ClientId==clientProfile$ClientId), aes(mean, sd), color="red", size=3) +
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

# Marginal density curve for mean values
xdensity <- ggplot(group, aes(mean, fill=c('#999999'))) + 
  geom_density(alpha=.5, adjust=2) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")

# Marginal density curve for sd values
ydensity <- ggplot(group, aes(sd, fill=c('#999999'))) + 
  geom_density(alpha=.5, adjust=2) + 
  coord_flip() +
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")

# Create blank plot to hold previous plots
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  labs(
    title = paste("Position du client dans le groupe #", S_S_I.client$S_S_I, "\nDe la segementation S_S_I", sep=""),
    subtitle = "Donnees generees a partir du dataset KaDo"
  )

# Create and save grid plot
png(file=paste(clientDir, "/S_S_I.png", sep=""),
    width=1200, height=700)
gridPlot <- grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
                         ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
dev.off()




# [S_F] Prefered product family
S_F <- read_csv(paste(projectPath, "03_output/S_F.csv", sep=""))  # Getting segmentation result

# Extracting clusterId and data
S_F.client <- subset(S_F, ClientId == clientId)                                # Extracting client entry
famillyId <- S_F.client$FamilleId                                              # Getting client prefered product family
n <- S_F.client$n                                                              # Getting number of bought product in the family

S_F.client <- S_F.client %>%                                                   
  select(ClientId, ClusterId) %>%                                              # Keeping only ClientId and ClusterId
  rename(S_F = ClusterId)                                                      # Renaming CluesterId column name to prepare merging

clientProfile <- clientProfile %>%
  merge(S_F.client, by="ClientId", sort=F)                                     # Merging ClusterId into the client profile

# Getting client position in group
orderedGroup <- S_F %>%
  filter(S_F$ClusterId==clientProfile$S_F) %>%
  arrange(desc(n))
pos <- match(n, orderedGroup$n)

# Getting group position and data
group <- S_F %>%
  filter(S_F$ClusterId==clientProfile$S_F)                                     # Keeping only data from the same cluster as the user's cluster
maxN  <-       max (group$n)
minN  <-       min (group$n)
meanN <- round(mean(group$n), digits=2)
sdN   <- round(sd  (group$n), digits=2)
famillyName <- subset(Families, FamilleId == famillyId)$Famille

# Writing segmentation analysis
write("- Famille de produits preferee", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe qui prefere la famille de produit ", famillyName, ", dont il a achete ", n, " produits.", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe a achete au maximum  ", maxN, " et au minimum ", minN, " produits de la famille ", famillyName, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec ", n, " produits achetes de la famille ", famillyName, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_F, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Produits achetes:     [", minN, ", ", n, ", ", maxN, "] (mean: ", meanN, ", sd: ", sdN, ")", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

group
# Generating plot
# Scatter plot of clients in the group
scatterPlot <- ggplot(group,aes(n, FamilleId)) + 
  geom_point() + 
  geom_point(data=group%>%filter(ClientId==clientProfile$ClientId), aes(n, FamilleId), color="red", size=3) +
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

# Marginal density curve for n values
xdensity <- ggplot(group, aes(n, fill=c('#999999'))) + 
  geom_density(alpha=.5, adjust=2) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")

# Create blank plot to hold previous plots
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

blankPlot_title <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  labs(
    title = paste("Position du client dans le groupe #", S_F.client$S_F, "\nDe la segementation S_F", sep=""),
    subtitle = "Donnees generees a partir du dataset KaDo"
  )


# Create and save grid plot
png(file=paste(clientDir, "/S_F.png", sep=""),
    width=1200, height=700)
gridPlot <- grid.arrange(xdensity, blankPlot_title, scatterPlot, blankPlot, 
                         ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
dev.off()



## CLIENT RECOMMENDATIONS ###############################
print("## Client recommendations")
write("----- RECOMMENDATIONS -----", file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

# Display function for recommendations
display_Recommendations <- function(descriptionFile, R_top_related_products, R_top_related, clientPreferedProductFamily, clientPreferedProducts, C_MF_Y, recommendation_intro, recommendation_description){
  
  # Making recommendation
  print(R_top_related_products)
  write(paste("- ", recommendation_intro, sep=""), file=descriptionFile, append=T)
  write(paste("    * ", R_top_related_products[1, "Libelle"], " (#", R_top_related_products[1, "ProduitId"], ") [", R_top_related_products[1, "Famille"], "]", sep=""), file=descriptionFile, append=T)
  write(paste("    * ", R_top_related_products[2, "Libelle"], " (#", R_top_related_products[2, "ProduitId"], ") [", R_top_related_products[2, "Famille"], "]", sep=""), file=descriptionFile, append=T)
  write(paste("    * ", R_top_related_products[3, "Libelle"], " (#", R_top_related_products[3, "ProduitId"], ") [", R_top_related_products[3, "Famille"], "]", sep=""), file=descriptionFile, append=T)
  
  # Explaining recommendation
  write(paste("    ", sep=""), file=descriptionFile, append=T)
  write(paste("    ", recommendation_description, sep=""), file=descriptionFile, append=T)
  
  # Details about the products
  write(paste("", sep=""), file=descriptionFile, append=T)
  write(paste("    Details sur les produits recommendes : ", sep=""), file=descriptionFile, append=T)
  for(i in 1:3){
    write(paste("        * ", R_top_related_products[i, "Libelle"], " (#", R_top_related_products[i, "ProduitId"], ") a ete achete ", R_top_related_products[i, "n"], " fois par des profils semblables au votres selon les criteres mentionnes ci-dessus.", sep=""), file=descriptionFile, append=T)
    write(paste("            Ce produit produit a ete selectionne parmis les produits potentiels car il fait parti des produits les plus populaires des profils proches du votres", ifelse(R_top_related_products[i, "Famille"]==clientPreferedProductFamily$Famille, " et qu'il fait parti de votre famille de produit preferee.", "."), sep=""), file=descriptionFile, append=T)  
    write(paste("", sep=""), file=descriptionFile, append=T)
  }
  
  # Details about the closest profils 
  write(paste("    Details sur les profils proches du votre (top 3) : ", sep=""), file=descriptionFile, append=T)
  for(i in 1:3){
    write(paste("        * Client #", R_top_related[i, "ClientId"], " est a une distance totale de votre profil de ", R_top_related[i, "dist"], ", ses produits preferes sont : ", sep=""), file=descriptionFile, append=T)
    produits <- C_MF_Y %>%
      filter(ClientId == R_top_related[i, "ClientId"]) %>%
      filter(!(ProduitId %in% clientPreferedProducts$ProduitId)) %>%
      select(ProduitId, n) %>%
      group_by(ProduitId) %>%
      summarise(n=sum(n)) %>%
      merge(Products, by='ProduitId', sort=F)
    for(j in 1:3){
      if(!is.na(produits[j,"n"])){
        write(paste("            * ", produits[j, "Libelle"], " (#", produits[j, "ProduitId"], ") achete ", produits[j, "n"], " fois et de la famille ", produits[j, "Famille"], sep=""), file=descriptionFile, append=T)  
      }
    }
    write(paste("", sep=""), file=descriptionFile, append=T)
  }
  write(paste("", sep=""), file=descriptionFile, append=T)
}


# Getting general data
C_MF_Y <- read_csv(paste(projectPath, "03_output/C_MF_Y.csv", sep=""))                                    # Getting client's preferred products
clientPreferedProducts <- C_MF_Y %>%
  filter(ClientId==clientId) %>%
  select(ProduitId)
clientPreferedProductFamily <- subset(S_F, ClientId==clientId) %>%
  merge(Families, by="FamilleId") %>%
  select(ClientId, FamilleId, Famille)


# Getting related clients for S_R segmentation 
client_S_R <- subset(S_R, ClientId == clientId)                                                           # Keeping only client's data
related_S_R <-  S_R %>%
  filter(S_R$ClusterId==clientProfile$S_R & S_R$ClientId!=clientProfile$ClientId) %>%                     # Keeping only data from the same cluster as the client's cluster and removing client itself
  transform(dist=round(sqrt(abs(mean-client_S_R$mean)^2+abs(sd-client_S_R$sd)^2), digits=2)) %>%          # Calculating distance with the selected client
  arrange(dist)                                                                                           # Ordering by distance ascending
  
# Getting related clients for S_S_T segmentation
client_S_S_T <- subset(S_S_T, ClientId == clientId)                                                       # Keeping only client's data
related_S_S_T <-  S_S_T %>%
  filter(S_S_T$ClusterId==clientProfile$S_S_T & S_S_T$ClientId!=clientProfile$ClientId) %>%               # Keeping only data from the same cluster as the client's cluster and removing client itself
  transform(dist=round(sqrt(abs(mean-client_S_S_T$mean)^2+abs(sd-client_S_S_T$sd)^2), digits=2)) %>%      # Calculating distance with the selected client
  arrange(dist)                                                                                           # Ordering by distance ascending

# Getting related clients for S_S_I segmentation
client_S_S_I <- subset(S_S_I, ClientId == clientId)                                                       # Keeping only client's data
related_S_S_I <-  S_S_I %>%
  filter(S_S_I$ClusterId==clientProfile$S_S_I & S_S_I$ClientId!=clientProfile$ClientId) %>%               # Keeping only data from the same cluster as the client's cluster and removing client itself
  transform(dist=round(sqrt(abs(mean-client_S_S_I$mean)^2+abs(sd-client_S_S_I$sd)^2), digits=2)) %>%      # Calculating distance with the selected client
  arrange(dist)                                                                                           # Ordering by distance ascending

# Getting related clients for S_F segmentation
client_S_F <- subset(S_F, ClientId == clientId)                                                           # Keeping only client's data
related_S_F <-  S_F %>%
  filter(S_F$ClusterId==clientProfile$S_F & S_F$ClientId!=clientProfile$ClientId) %>%                     # Keeping only data from the same cluster as the client's cluster and removing client itself
  transform(dist=round(abs((n-client_S_F$n)/12), digits=2)) %>%                                             # Calculating distance with the selected client (divinding by 12 to keep indicator normalized with the others segmentations)
  arrange(dist)                                                                                           # Ordering by distance ascending




### [R_1] "Other profiles like yours also like..." ############################### 
# Calculating how close each profile that appears at least once in the same cluster are from the selected client
R_1_related <- related_S_R %>%
  rename(dist_S_R=dist) %>%
  select(ClientId, dist_S_R) %>%
  merge(related_S_S_T %>%
          rename(dist_S_S_T=dist) %>%
          select(ClientId, dist_S_S_T),
        by="ClientId", sort=F, all = TRUE
  ) %>%
  merge(related_S_S_I %>%
          rename(dist_S_S_I=dist) %>%
          select(ClientId, dist_S_S_I),
        by="ClientId", sort=F, all = TRUE
  ) %>%
  merge(related_S_F %>%
          rename(dist_S_F=dist) %>%
          select(ClientId, dist_S_F),
        by="ClientId", sort=F, all = TRUE
  ) %>%
  transform(dist_S_R  =ifelse(is.na(dist_S_R),   max(related_S_R$dist),    dist_S_R    )) %>%
  transform(dist_S_S_T=ifelse(is.na(dist_S_S_T), max(related_S_S_T$dist),  dist_S_S_T  )) %>%
  transform(dist_S_S_I=ifelse(is.na(dist_S_S_I), max(related_S_S_I$dist),  dist_S_S_I  )) %>%
  transform(dist_S_F  =ifelse(is.na(dist_S_F),   max(related_S_F$dist),    dist_S_F    )) %>%
  transform(dist=round(dist_S_R+dist_S_S_T+dist_S_S_I+dist_S_F, digits=2)) %>%
  arrange(dist)
  
# Getting closest scores
R_1_top_scores <- R_1_related %>%
  select(dist) %>%
  distinct() %>%
  head(3)

# Getting closest profiles
R_1_top_related <- R_1_related %>%
  filter(dist %in% R_1_top_scores$dist)

# Getting closest profile's preferred products
R_1_top_related_products <- C_MF_Y %>%
  filter(ClientId %in% R_1_top_related$ClientId) %>%
  filter(!(ProduitId %in% clientPreferedProducts$ProduitId)) %>%
  select(ProduitId, n) %>%
  group_by(ProduitId) %>%
  summarise(n=sum(n)) %>%
  merge(Products, by='ProduitId', sort=F) %>%
  arrange(match(Famille, clientPreferedProductFamily$Famille), desc(n))

# Displaying recommendation
display_Recommendations(descriptionFile, R_1_top_related_products, R_1_top_related, clientPreferedProductFamily, clientPreferedProducts, C_MF_Y, "D'autres profils comme le votre aiment aussi...", "Ces recommendations se basent sur les produits preferes des profils les plus proches du votre suivant la regularite, les depenses par achats et par produits ainsi que la famille de produit preferee pour vous proposer des produits dans votre budget et centre d'interet")


### [R_2] "Because you are interested in..." ###############################
# Calculating how close each profile that appears at least once in the same cluster are from the selected client
R_2_related <- related_S_F %>%
  select(ClientId, dist) %>%
  arrange(dist)

# Getting closest scores
R_2_top_scores <- R_2_related %>%
  select(dist) %>%
  distinct() %>%
  head(3)


# Getting closest profiles
R_2_top_related <- R_2_related %>%
  filter(dist %in% R_2_top_scores$dist)

# Getting closest profile's preferred products
R_2_top_related_products <- C_MF_Y %>%
  filter(ClientId %in% R_2_top_related$ClientId) %>%
  filter(!(ProduitId %in% clientPreferedProducts$ProduitId)) %>%
  select(ProduitId, n) %>%
  group_by(ProduitId) %>%
  summarise(n=sum(n)) %>%
  merge(Products, by='ProduitId', sort=F) %>%
  arrange(match(Famille, clientPreferedProductFamily$Famille), desc(n))

# Displaying recommendation
display_Recommendations(descriptionFile, R_2_top_related_products, R_2_top_related, clientPreferedProductFamily, clientPreferedProducts, C_MF_Y, "Parce que vous etes interesse par...", "Ces recommendations se basent sur le type de produits que vous preferez et sur le succes de produits de ce type pour vous proposer de nouveaux produits dans votre centre d'interet")


### [R_3] "Based on your budget..." ###############################
# Calculating how close each profile that appears at least once in the same cluster are from the selected client
R_3_related <- related_S_S_T %>%
  rename(dist_S_S_T=dist) %>%
  select(ClientId, dist_S_S_T) %>%
  merge(related_S_S_I %>%
          rename(dist_S_S_I=dist) %>%
          select(ClientId, dist_S_S_I),
        by="ClientId", sort=F, all = TRUE
  ) %>%
  transform(dist_S_S_T=ifelse(is.na(dist_S_S_T), max(related_S_S_T$dist), dist_S_S_T)) %>%
  transform(dist_S_S_I=ifelse(is.na(dist_S_S_I), max(related_S_S_I$dist), dist_S_S_I)) %>%
  transform(dist=round(dist_S_S_T+dist_S_S_I, digits=2)) %>%
  arrange(dist)

# Getting closest scores
R_3_top_scores <- R_3_related %>%
  select(dist) %>%
  distinct() %>%
  head(3)

# Getting closest profiles
R_3_top_related <- R_3_related %>%
  filter(dist %in% R_3_top_scores$dist)

# Getting closest profile's preferred products
R_3_top_related_products <- C_MF_Y %>%
  filter(ClientId %in% R_3_top_related$ClientId) %>%
  filter(!(ProduitId %in% clientPreferedProducts$ProduitId)) %>%
  select(ProduitId, n) %>%
  group_by(ProduitId) %>%
  summarise(n=sum(n)) %>%
  merge(Products, by='ProduitId', sort=F) %>%
  arrange(match(Famille, clientPreferedProductFamily$Famille), desc(n))

# Displaying recommendation
display_Recommendations(descriptionFile, R_3_top_related_products, R_3_top_related, clientPreferedProductFamily, clientPreferedProducts, C_MF_Y, "Base sur votre budget, nous vous recommandons...", "Ces recommendations se basent sur les produits preferes des profils les plus proches du votre suivant les depenses par achats et par produits pour vous proposer des produits dans votre budget et centre d'interet")

