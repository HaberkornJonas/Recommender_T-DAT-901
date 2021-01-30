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

orderedGoups

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
write(paste("    Groupe ID:            [", clientProfile$S_F, "]", sep=""), file=descriptionFile, append=T)
write(paste("    Produits achetes:     [", minN, ", ", n, ", ", maxN, "]", sep=""), file=descriptionFile, append=T)
write(paste("", sep=""), file=descriptionFile, append=T)






## CLIENT RECOMMENDATIONS ###############################
print("## Client recommendations")
write("----- CLIENT RECOMMENDATIONS -----", file=descriptionFile, append=T)

# [R_1] "Other profiles like yours also like..." 
# TODO

# [R_2] "Because you are interested in..." 
# TODO

# [R_3] "Based on your budget..." 
# TODO



## CLEAN UP ###############################

