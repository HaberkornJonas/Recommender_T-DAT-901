## IMPORTS ###############################
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(Hmisc)
library(tidyr)


## STATIC VALUES ###############################
projectPath <- "C:/Users/jonas/Desktop/T-DAT/"
moisIds <- c(1:12)
moisNoms <- c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
moisDictionary <- data.frame(moisVenteId=c(1:12), moisVente=moisNoms)


Customers <- read_csv(paste(projectPath, "03_output/Customers.csv", sep=""))
Families <- read_csv(paste(projectPath, "03_output/Familles.csv", sep=""))
Products <- read_csv(paste(projectPath, "03_output/Produits.csv", sep=""))


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


## CLIENT STATISTICAL DESCRIPTION ###############################
print("## Client statistical dedscripton")
write("----- STATISTICAL DESCRIPTION -----", file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

# Client ID
write(paste("ClientId:",clientId), file=descriptionFile, append=T)

# Add lots of statistics (# tickets, 3 most frequented months, money spend, # products bought, etc.)
# TODO


write(paste("", sep=""), file=descriptionFile, append=T)



## CLIENT SEGMENTATION ANALYSIS ###############################
print("## Client segmentation analysis")
write("----- SEGMENTATION ANALYSIS -----", file=descriptionFile, append=T)
clientProfile <- data.frame(ClientId=clientId)



## [S_R] Regularity
write(paste("", sep=""), file=descriptionFile, append=T)
S_R <- Customers <- read_csv(paste(projectPath, "03_output/S_R.csv", sep=""))  # Getting segmentation result

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
maxMean <- max(group$mean)
minMean <- min(group$mean)
maxSD <- max(group$sd)
minSD <- min(group$sd)
maxDist <- max(orderedGroup$dist)
minDist <- min(orderedGroup$dist)


# Writing segmentation analysis
write("- Regularite (vient souvent ou non)", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe en position ", groupPos, " sur ", count(orderedGoups)$n, " avec une regularite moyenne de ", mean, " tickets par mois et une deviation standard de ", sd, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe possede une moyenne maximum de ", maxMean, " et minimum de ", minMean, " tickets par mois pour une deviation standard maximum de ", maxSD, " et minimum de ", minSD, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec une distance a l'origine de ", dist, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_R, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Moyenne:              [", minMean, ", ", mean, ", ", maxMean, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Deviation standard:   [", minSD,   ", ", sd,   ", ", maxSD,   "]", sep=""), file=descriptionFile, append=T)
write(paste("    Distance a l'origine: [", minDist, ", ", dist, ", ", maxDist, "]", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)





# [S_S_T] Spendings per tickets
S_S_T <- Customers <- read_csv(paste(projectPath, "03_output/S_S_T.csv", sep=""))  # Getting segmentation result

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
maxMean <- max(group$mean)
minMean <- min(group$mean)
maxSD <- max(group$sd)
minSD <- min(group$sd)
maxDist <- max(orderedGroup$dist)
minDist <- min(orderedGroup$dist)


# Writing segmentation analysis
write("- Depenses par ticket (depenses beaucoup ou non)", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe en position ", groupPos, " sur ", count(orderedGoups)$n, " avec une depense par ticket moyenne de ", mean, " et une deviation standard de ", sd, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe possede une moyenne de depenses par ticket maximum de ", maxMean, " et minimum de ", minMean, " pour une deviation standard maximum de ", maxSD, " et minimum de ", minSD, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec une distance a l'origine de ", dist, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_S_T, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Moyenne:              [", minMean, ", ", mean, ", ", maxMean, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Deviation standard:   [", minSD,   ", ", sd,   ", ", maxSD,   "]", sep=""), file=descriptionFile, append=T)
write(paste("    Distance a l'origine: [", minDist, ", ", dist, ", ", maxDist, "]", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)





# [S_S_I] Spendings per items
S_S_I <- Customers <- read_csv(paste(projectPath, "03_output/S_S_I.csv", sep=""))  # Getting segmentation result

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
maxMean <- max(group$mean)
minMean <- min(group$mean)
maxSD <- max(group$sd)
minSD <- min(group$sd)
maxDist <- max(orderedGroup$dist)
minDist <- min(orderedGroup$dist)

# Writing segmentation analysis
write("- Depenses par produit (achete des produits chere ou non)", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe en position ", groupPos, " sur ", count(orderedGoups)$n, " avec une depense par produit moyenne de ", mean, " et une deviation standard de ", sd, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe possede une moyenne de depenses par produit maximum de ", maxMean, " et minimum de ", minMean, " pour une deviation standard maximum de ", maxSD, " et minimum de ", minSD, ". (en euros)", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec une distance a l'origine de ", dist, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_S_I, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Moyenne:              [", minMean, ", ", mean, ", ", maxMean, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Deviation standard:   [", minSD,   ", ", sd,   ", ", maxSD,   "]", sep=""), file=descriptionFile, append=T)
write(paste("    Distance a l'origine: [", minDist, ", ", dist, ", ", maxDist, "]", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)





# [S_F] Prefered product family
S_F <- Customers <- read_csv(paste(projectPath, "03_output/S_F.csv", sep=""))  # Getting segmentation result

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
maxN <- max(group$n)
minN <- min(group$n)
famillyName <- subset(Families, FamilleId == famillyId)$Famille

# Writing segmentation analysis
write("- Famille de produits preferee", file=descriptionFile, append=T)
write(paste("    Le client fait parti du groupe qui prefere la famille de produit ", famillyName, ", dont il a achete ", n, " produits.", sep=""), file=descriptionFile, append=T)
write(paste("    Le groupe a achete au maximum  ", maxN, " et au minimum ", minN, " produits de la famille ", famillyName, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Le client se place donc en position ", pos, " sur ", count(orderedGroup)$n, " de son groupe avec ", n, " produits achetes de la famille ", famillyName, ".", sep=""), file=descriptionFile, append=T)
write(paste("    Groupe ID:            [", clientProfile$S_F, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Produits achetes:     [", minN, ", ", n, ", ", maxN, "]", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)






## CLIENT RECOMMENDATIONS ###############################
print("## Client recommendations")
write("----- CLIENT RECOMMENDATIONS -----", file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

# Getting general data
C_MF_Y <- read_csv(paste(projectPath, "03_output/C_MF_Y.csv", sep=""))                                    # Getting client's preferred products
C_MF_Y <- read_csv(paste(projectPath, "03_output/C_MF_Y.csv", sep=""))                                    # Getting client's preferred products
clientPreferedProducts <- C_MF_Y %>%
  filter(ClientId==clientId) %>%
  select(ProduitId)
clientPreferedProductFamily <- subset(S_F, ClientId==clientId) %>%
  merge(Familles, by="FamilleId") %>%
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
  transform(dist=round(abs(n-client_S_F$n/12), digits=2)) %>%                                             # Calculating distance with the selected client (divinding by 12 to keep indicator normalized with the others segmentations)
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
  transform(dist_S_R  =ifelse(is.na(dist_S_R),   max(related_S_R$dist),   dist_S_R  )) %>%
  transform(dist_S_S_T=ifelse(is.na(dist_S_S_T), max(related_S_S_T$dist), dist_S_S_T)) %>%
  transform(dist_S_S_I=ifelse(is.na(dist_S_S_I), max(related_S_S_I$dist), dist_S_S_I)) %>%
  transform(dist_S_F  =ifelse(is.na(dist_S_F),   max(related_S_F$dist),   dist_S_F  )) %>%
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

# Making recommendation
write("- D'autres profils comme le votre aiment aussi...", file=descriptionFile, append=T)
write(paste("    * ", R_1_top_related_products[1, "Libelle"], " (#", R_1_top_related_products[1, "ProduitId"], ")", sep=""), file=descriptionFile, append=T)
write(paste("    * ", R_1_top_related_products[2, "Libelle"], " (#", R_1_top_related_products[2, "ProduitId"], ")", sep=""), file=descriptionFile, append=T)
write(paste("    * ", R_1_top_related_products[3, "Libelle"], " (#", R_1_top_related_products[3, "ProduitId"], ")", sep=""), file=descriptionFile, append=T)
write(paste("    (Ces recommendations se basent sur les produits preferes des profils les plus proches du votre suivant la regularite, les depenses par achats et par produits ainsi que la famille de produit preferee pour vous proposer des produits dans votre budget et centre d'interet)", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)



### [R_2] "Because you are interested in..." ###############################
#TODO



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


# Making recommendation
write("- Base sur votre budget, nous vous recommandons...", file=descriptionFile, append=T)
write(paste("    * ", R_3_top_related_products[1, "Libelle"], " (#", R_3_top_related_products[1, "ProduitId"], ")", sep=""), file=descriptionFile, append=T)
write(paste("    * ", R_3_top_related_products[2, "Libelle"], " (#", R_3_top_related_products[2, "ProduitId"], ")", sep=""), file=descriptionFile, append=T)
write(paste("    * ", R_3_top_related_products[3, "Libelle"], " (#", R_3_top_related_products[3, "ProduitId"], ")", sep=""), file=descriptionFile, append=T)
write(paste("    (Ces recommendations se basent sur les produits preferes des profils les plus proches du votre suivant les depenses par achats et par produits pour vous proposer des produits dans votre budget et centre d'interet)", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)

