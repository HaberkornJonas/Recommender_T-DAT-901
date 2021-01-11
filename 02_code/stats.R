## IMPORTS ###############################
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(Hmisc)

## INFO: 
##    - ggplot tuto: https://r4ds.had.co.nz/data-visualisation.html


## STATIC VALUES ###############################
projectPath <- "~/Projects/School/TDAT901/t-dat-901/"
moisIds <- c(1:12)
moisNoms <- c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
moisDictionary <- data.frame(moisVenteId=c(1:12), moisVente=moisNoms)


## READING DATASET ###############################
print("Lecture des donees...")

sourceData <- read_csv(paste(projectPath, "01_data/KaDo.csv", sep="")) %>%
              rename(
                  TicketId = TICKET_ID,
                  MoisVenteId = MOIS_VENTE,
                  PrixNet = PRIX_NET,
                  Famille = FAMILLE,
                  Univers = UNIVERS,
                  Maille = MAILLE,
                  Libelle = LIBELLE,
                  ClientId = CLI_ID
              )
print("Done!")

### Clearing Data, Removing those 2 Families because they are not relevant ###
sourceData <- as.data.frame(sourceData)
sourceData <- sourceData[!(sourceData$Famille=="MULTI FAMILLES"),]
sourceData <- sourceData[!(sourceData$Famille=="SANTE NATURELLE"),]

####  [G] General habits and facts ####
####  [G_T] General tickets stats  ####
## [G_T_Y_T] ############################### 
print("[G_T_Y_T]...")

# Manipulation des donnees
G.T.Y.T <- sourceData$TicketId %>%                                          # Use Ticket column
           unique() %>%                                                     # Get unique values (# different tickets)
           length() %>%                                                     # Get the number of unique values
           data.frame()                                                     # Make it a dataframe
colnames(G.T.Y.T) <- c("n")                                                 # Give column name for CSV

# Sauvegarde des donnees
write_csv(G.T.Y.T, paste(projectPath, "03_output/G.T.Y.T.csv", sep=""))

# Creation du graphique
df <- data.frame(x = 1, y = G.T.Y.T[1,1])
plot <- ggplot(df, aes(x=x,y=y))+
  geom_segment( aes(x=x, xend=x, y=0, yend=y), size=1.3, alpha=0.9)+
  labs(
    title = "Nombre total de tickets",
    subtitle = "Donnees du dataset KaDo",
    x = "Annee",
    y = "Nombre de tickets"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
print(plot)

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/G.T.Y.T.png", sep=""), width = 5, height = 8, dpi = 100)
sprintf("[G_T_Y_T] There was a total of %s tickets for this year.", G.T.Y.T[1,1])









## [G_T_M_T] ############################### 
print("[G_T_M_T]...")

# Manipulation des donnees
G.T.M.T <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G.T.M.T <- aggregate(G.T.M.T$TicketId, by=list(G.T.M.T$MoisVenteId), FUN=length) %>%   # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns

# Sauvegarde des donnees
write_csv(G.T.M.T, paste(projectPath, "03_output/G.T.M.T.csv", sep=""))



# Creation du graphique
df <- data.frame(x = 1, y = G.T.M.T[1,1])
plot <-  ggplot(data = G.T.M.T, aes(x = as.numeric(MoisVenteId), y=n))+
  geom_histogram(stat="identity", fill="steelblue3", color="steelblue3")+
  geom_errorbar(aes(ymin=n-sd(G.T.M.T$n), ymax=n+sd(G.T.M.T$n)), width=.2,
                position=position_dodge(.9))+
  labs(
    title = "Nombre total de tickets",
    subtitle = "Donnees du dataset KaDo",
    x = "Annee",
    y = "Nombre de tickets"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
print(plot)


# Creation du graphique
plot <- ggplot(data = G.T.M.T, aes(x = as.numeric(MoisVenteId), y = n))+
  geom_histogram(stat="identity", fill="steelblue3", color="gray40")+
  labs(
    title = "Nombre total de tickets par mois",
    subtitle = "Donnees du dataset KaDo",
    x = "Mois",
    y = "Nombre de tickets"
  )+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(
    breaks = moisIds,
    label = moisNoms
  )
print(plot)

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/G.T.M.T.png", sep=""), width = 12, height = 8, dpi = 100)
print("[G_T_M_T] Done!")











## [G_T_M_M] ############################### 
print("[G_T_M_M]...")

# Manipulation des donnees
G.T.M.M <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G.T.M.M <- aggregate(G.T.M.M$TicketId, by=list(G.T.M.M$MoisVenteId), FUN=length) %>%   # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns
G.T.M.M <- mean(G.T.M.M$n) %>%                                                         # Get mean value
           data.frame()                                                                # Make it a dataframe
colnames(G.T.M.M) <- c("mean")                                                         # Give column name for CSV

# Sauvegarde des donnees
write_csv(G.T.M.M, paste(projectPath, "03_output/G.T.M.M.csv", sep=""))

# Creation du graphique
df <- data.frame(x = 1, y = G.T.M.M[1,1])
plot <- ggplot(df, aes(x=x,y=y))+
  geom_segment( aes(x=x, xend=x, y=0, yend=y), size=1.3, alpha=0.9) +
  labs(
    title = "Nombre moyen de tickets par mois",
    subtitle = "Donnees du dataset KaDo",
    x = "Annee",
    y = "Nombre moyen de tickets par mois"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
print(plot)

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/G.T.M.M.png", sep=""), width = 5, height = 8, dpi = 100)
sprintf("[G_T_M_M] Done!")











## [G_T_M_S] ############################### 
print("[G_T_M_S]...")

# Manipulation des donnees
G.T.M.S <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G.T.M.S <- aggregate(G.T.M.S$TicketId, by=list(G.T.M.S$MoisVenteId), FUN=length) %>%   # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns
G.T.M.S <- sd(G.T.M.S$n) %>%                                                           # Get standard deviation
           data.frame()                                                                # Make it a dataframe
colnames(G.T.M.S) <- c("sd")                                                           # Give column name for CSV

# Sauvegarde des donnees
write_csv(G.T.M.S, paste(projectPath, "03_output/G.T.M.S.csv", sep=""))

# Creation du graphique
df <- data.frame(x = 1, y = G.T.M.S[1,1])
plot <- ggplot(df, aes(x=x,y=y))+
  geom_segment( aes(x=x, xend=x, y=0, yend=y), size=1.3, alpha=0.9) +
  labs(
    title = "Deviation standard du nombre de tickets par mois",
    subtitle = "Donnees du dataset KaDo",
    x = "Annee",
    y = "Deviation standard du nombre de tickets par mois"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
print(plot)

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/G.T.M.S.png", sep=""), width = 5, height = 8, dpi = 100)
sprintf("[G_T_M_S] Done!")












## [G_T_M] ############################### 
print("[G_T_M]...")

# Manipulation des donnees
mean <- G.T.M.M$mean
sd <- G.T.M.S$sd
G.T.M <- data.frame(mean, sd)

# Creation du graphique
df <- data.frame(x=1, y=G.T.M$mean)
plot <- ggplot(df, aes(x=x, y=y))+
  geom_bar(stat="identity", width=0.5, fill="steelblue3", color="gray40")+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.3, position=position_dodge(.9))+
  labs(
    title = "Nombre moyen de tickets par mois (avec deviation standard)",
    subtitle = "Donnees du dataset KaDo",
    y = "Nombre de tickets"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
print(plot)

# Sauvegarde du graphique
ggsave(paste(projectPath, "03_output/G.T.M.png", sep=""), width = 7, height = 8, dpi = 100)
sprintf("[G_T_M] Done!")











####  [G_S] General spendings stats ####
## [G_S_Y_T] ############################### 
# TODO


## [G_S_M_T] ############################### 
# TODO


## [G_S_M_M] ############################### 
# TODO


## [G_S_M_S] ############################### 
# TODO







####  [G_P] General Products stats ####
## [G_P_Y_T] ############################### 
# TODO


## [G_P_M_T] ############################### 
# TODO


## [G_P_M_M] ############################### 
# TODO


## [G_P_M_S] ############################### 
# TODO







####  [G_F] General Families stats ####

#### Products dataset creation ########

productsData = subset(sourceData, select = -c(TicketId, MoisVenteId, ClientId, PrixNet))
productsData <- as.data.frame(unique(G_F_T))

## [G_F_T] ############################### 
productsByFamily = as.data.frame(table(productsData$Famille))
colnames(productsByFamily)[1] = "Famille"
colnames(productsByFamily)[2] = "Produits"


productsByFamily_Plot <- ggplot(productsByFamily,
                     aes(x = Famille, y=Produits)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Nombre de produits par Famille"
  )

print(productsByFamily_Plot)

## [G_F_Y_T] ############################### 
G_F_Y_T <- as.data.frame(table(sourceData$Famille))
colnames(G_F_Y_T)[1] <- "Famille"
colnames(G_F_Y_T)[2] <- "Ventes"

G_F_Y_T_Plot <- ggplot(G_F_Y_T, 
               aes(x=Famille, y=Ventes)) +
        geom_bar(stat="identity") +
        labs(
          title = "Vente sur l'année par Famille"
        )

print(G_F_Y_T_Plot)

## [G_F_M_T] ############################### 
# TODO


## [G_F_M_M] ############################### 
# TODO


## [G_F_M_S] ############################### 
# TODO


## [G_F_P_M] ###############################
# Gettings all the sells for the products and removing useless colums
productsWithPriceDF = subset(sourceData, select = -c(TicketId, MoisVenteId, ClientId))

# Adding Prix Column to our existings dataframes for the items
bestCapillaryItems[, "Prix"] <- NA
bestHygieneItems[,"Prix"] <- NA
bestMakeupItems[, "Prix"] <- NA
bestParfumItems[, "Prix"] <- NA
bestBodyCareItems[, "Prix"] <- NA
bestFaceCareItems[, "Prix"] <- NA
bestSunItems[, "Prix"] <- NA

# Applying a correct price for every product in the capillary family
for (product in bestCapillaryItems$Libellé)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestCapillaryItems[bestCapillaryItems$Libellé==product,]$Prix = productFrequentPrice
}

# Applying a correct price for every product in the hygiene family
for (product in bestHygieneItems$Libellé)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestHygieneItems[bestHygieneItems$Libellé==product,]$Prix = productFrequentPrice
}

# Applying a correct price for every product in the makeup family
for (product in bestMakeupItems$Libellé)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestMakeupItems[bestMakeupItems$Libellé==product,]$Prix = productFrequentPrice
}

# Applying a correct price for every product in the parfume family
for (product in bestParfumItems$Libellé)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestParfumItems[bestParfumItems$Libellé==product,]$Prix = productFrequentPrice
}

# Applying a correct price for every product in the body care family
for (product in bestBodyCareItems$Libellé)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestBodyCareItems[bestBodyCareItems$Libellé==product,]$Prix = productFrequentPrice
}

# Applying a correct price for every product in the face care family
for (product in bestFaceCareItems$Libellé)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestFaceCareItems[bestFaceCareItems$Libellé==product,]$Prix = productFrequentPrice
}

# Applying a correct price for every product in the sun care family
for (product in bestSunItems$Libellé)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestSunItems[bestSunItems$Libellé==product,]$Prix = productFrequentPrice
}

# Means of prices of items in each categories
mean(bestCapillaryItems[["PRIX"]])
mean(bestHygieneItems[["PRIX"]])
mean(bestMakeupItems[["PRIX"]])
mean(bestParfumItems[["PRIX"]])
mean(bestBodyCareItems[["PRIX"]])
mean(bestFaceCareItems[["PRIX"]])
mean(bestSunItems[["PRIX"]])

## [G_F_P_S] ############################### 
# TODO







####  [G_U] General Univers stats ####
## [G_U_T] ############################### 
productsByUnivers = as.data.frame(table(productsData$Univers))
colnames(productsByUnivers)[1] = "Univers"
colnames(productsByUnivers)[2] = "Produits"


productsByUnivers_Plot <- ggplot(productsByUnivers,
                                aes(x = Univers, y=Produits)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Nombre de produits par univers"
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(productsByUnivers_Plot)

## [G_U_Y_T] ############################### 
# TODO


## [G_U_M_T] ############################### 
# TODO


## [G_U_M_M] ############################### 
# TODO


## [G_U_M_S] ############################### 
# TODO


## [G_U_P_M] ############################### 
# TODO


## [G_U_P_S] ############################### 
# TODO







####  [G_M] General Maille stats ####
## [G_M_T] ############################### 
productsByMaille = as.data.frame(table(productsData$Maille))
colnames(productsByMaille)[1] = "Maille"
colnames(productsByMaille)[2] = "Produits"


productsByMaille_Plot <- ggplot(productsByMaille,
                                 aes(x = Maille, y=Produits)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Nombre de produits par maille"
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(productsByMaille_Plot)


## [G_M_Y_T] ############################### 
# TODO


## [G_M_M_T] ############################### 
# TODO


## [G_M_M_M] ############################### 
# TODO


## [G_M_M_S] ############################### 
# TODO


## [G_M_P_M] ############################### 
# TODO


## [G_M_P_S] ############################### 
# TODO







####  [G_MF] General Most favorites Products ####
## [G_MF_Y] ############################### 
# TODO


## [G_MF_M] ############################### 
# TODO


## [G_MF_F] ############################### 
################################################
#     Getting best selling items by Family     #
################################################
# Hygiene Family #
hygieneFam = table(sourceData[sourceData$Famille=="HYGIENE", ]$Libelle)
bestHygieneItems = as.data.frame(hygieneFam[order(hygieneFam, decreasing = TRUE)])
colnames(bestHygieneItems)[1] <- "Libellé"
colnames(bestHygieneItems)[2] <- "Ventes"
write_csv(bestHygieneItems, paste(projectPath, "03_output/bestHygieneItems.csv", sep=""))

# Capillary Family #
capillaryFam = table(sourceData[sourceData$Famille=="CAPILLAIRES", ]$Libelle)
bestCapillaryItems = as.data.frame(capillaryFam[order(capillaryFam, decreasing = TRUE)])
colnames(bestCapillaryItems)[1] <- "Libellé"
colnames(bestCapillaryItems)[2] <- "Ventes"
write_csv(bestCapillaryItems, paste(projectPath, "03_output/bestCapillaryItems.csv", sep=""))

# Makeup Family #
makeupFam = table(sourceData[sourceData$Famille=="MAQUILLAGE", ]$Libelle)
bestMakeupItems = as.data.frame(makeupFam[order(makeupFam, decreasing = TRUE)])
colnames(bestMakeupItems)[1] <- "Libellé"
colnames(bestMakeupItems)[2] <- "Ventes"
write_csv(bestMakeupItems, paste(projectPath, "03_output/bestMakeupItems.csv", sep=""))

# Parfum Family #
parfumFam = table(sourceData[sourceData$Famille=="PARFUMAGE", ]$Libelle)
bestParfumItems = as.data.frame(parfumFam[order(parfumFam, decreasing = TRUE)])
colnames(bestParfumItems)[1] <- "Libellé"
colnames(bestParfumItems)[2] <- "Ventes"
write_csv(bestParfumItems, paste(projectPath, "03_output/bestParfumItems.csv", sep=""))

# Body Care Family #
bodyCareFam = table(sourceData[sourceData$Famille=="SOINS DU CORPS", ]$Libelle)
bestBodyCareItems = as.data.frame(bodyCareFam[order(bodyCareFam, decreasing = TRUE)])
colnames(bestBodyCareItems)[1] <- "Libellé"
colnames(bestBodyCareItems)[2] <- "Ventes"
write_csv(bestBodyCareItems, paste(projectPath, "03_output/bestBodyCareItems.csv", sep=""))

# Face Care Family #
faceCareFam = table(sourceData[sourceData$Famille=="SOINS DU VISAGE", ]$Libelle)
bestFaceCareItems = as.data.frame(faceCareFam[order(faceCareFam, decreasing = TRUE)])
colnames(bestFaceCareItems)[1] <- "Libellé"
colnames(bestFaceCareItems)[2] <- "Ventes"
write_csv(bestFaceCareItems, paste(projectPath, "03_output/bestFaceCareItems.csv", sep=""))

# Sun Family #
sunFam = table(sourceData[sourceData$Famille=="SOLAIRES", ]$Libelle)
bestSunItems = as.data.frame(sunFam[order(sunFam, decreasing = TRUE)])
colnames(bestSunItems)[1] <- "Libellé"
colnames(bestSunItems)[2] <- "Ventes"
write_csv(bestSunItems, paste(projectPath, "03_output/bestSunItems.csv", sep=""))

####  [G_C] General Customer stats ####
####  [G_C_T] General Customer Ticket stats ####
## [G_C_T_Y_M] ############################### 
# TODO


## [G_C_T_Y_S] ############################### 
# TODO


## [G_C_T_M_M] ############################### 
# TODO

## [G_C_P_Y_M] ###############################  
itemsBoughtByClients = as.data.frame(sort(table(sourceData$ClientId), decreasing=TRUE))
colnames(itemsBoughtByClients)[1] <- "ClientID"
colnames(itemsBoughtByClients)[2] <- "ItemsBought"
mean(itemsBoughtByClients[["ItemsBought"]])

## [G_C_P_Y_S] ############################### 
sd(itemsBoughtByClients[["ItemsBought"]])

## [G_C_T_M_S] ############################### 
# TODO





####  [G_C_S] General Customer Spendings stats ####
## [G_C_S_Y_M] ############################### 
# TODO


## [G_C_S_Y_S] ############################### 
# TODO


## [G_C_S_M_M] ############################### 
# TODO


## [G_C_S_M_S] ############################### 
# TODO




####  [G_C_S_T] General Customer Spendings per ticket stats ####
## [G_C_S_T_Y_M] ############################### 
# TODO


## [G_C_S_T_Y_S] ############################### 
# TODO


## [G_C_S_T_M_M] ############################### 
# TODO


## [G_C_S_T_M_S] ############################### 
# TODO




####  [G_C_S_F] General Customer Spendings per product family stats ####
## [G_C_S_F_Y_M] ############################### 
# TODO


## [G_C_S_F_Y_S] ############################### 
# TODO


## [G_C_S_F_M_M] ############################### 
# TODO


## [G_C_S_F_M_S] ############################### 
# TODO






####  [G_C_F] General product family stats ####
## [G_C_F_Y_M] ############################### 
# TODO


## [G_C_F_Y_S] ############################### 
# TODO








####  [C] Customer habits and facts ####
####  [C_T] Customer tickets stats  ####
## [C_T_Y_T] ############################### 
# TODO


## [C_T_M_T] ############################### 
# TODO


## [C_T_M_M] ############################### 
# TODO


## [C_T_M_S] ############################### 
# TODO






####  [C_S] Customer spending stats  ####
## [C_S_Y_T] ############################### 
# TODO


## [C_S_M_T] ############################### 
# TODO


## [C_S_M_M] ############################### 
# TODO


## [C_S_M_S] ############################### 
# TODO


## [C_S_T_M] ############################### 
# TODO


## [C_S_T_S] ############################### 
# TODO


## [C_S_P_M] ############################### 
# TODO


## [C_S_P_S] ############################### 
# TODO








####  [C_P] Customer Products bought stats  ####
## [C_P_Y_T] ############################### 
# TODO


## [C_P_M_T] ############################### 
# TODO


## [C_P_M_M] ############################### 
# TODO





####  [C_C] Customer characteristics  ####
## [C_C_G] ############################### 
# TODO






####  [C_F] Customer product families stats  ####
## [C_F_Y_T] ############################### 
# TODO


## [C_F_M_T] ############################### 
# TODO


## [C_F_M_M] ############################### 
# TODO


## [C_F_M_S] ############################### 
# TODO




####  [C_F_S] Customer spendings per product families stats  ####
## [C_F_S_Y_M] ############################### 
# TODO


## [C_F_S_Y_S] ############################### 
# TODO






####  [C_F_MF] Customer most favorite product per families stats  ####
## [C_F_MF_Y] ############################### 
# TODO





####  [C_F_LF] Customer least favorite product per families stats  ####
## [C_F_LF_Y] ############################### 
# TODO





####  [C_MF] Customer most favorite product stats  ####
## [C_MF_Y] ############################### 
# TODO


## [C_MF_M] ############################### 
# TODO





####  [C_LF] Customer least favorite product stats  ####
## [C_LF_Y] ############################### 
# TODO


## [C_LF_M] ############################### 
# TODO

















## OLD STATS - Will be deleted ####

##   TOTAL DE VENTES PAR MOIS 
print("Analyse du nombre total de ventes par mois...")

# Manipulation des donnees
ventes_total_par_mois <- sourceData %>%
  group_by(mois_vente_id) %>%
  count() %>%
  rename(nombre_ventes = n)

# Sauvegarde des données
write_csv(ventes_total_par_mois, "03_output/01_ventes_total_par_mois.csv")

# Creation du graphique
plot <- ggplot(data = ventes_total_par_mois, aes(x = mois_vente_id, y = nombre_ventes))+
  geom_bar(stat="identity", fill="steelblue3", color="steelblue3")+
  labs(
    title = "Nombre total de ventes par mois",
    subtitle = "Données du dataset KaDo",
    x = "Mois",
    y = "Nombre de ventes"
  )+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(
    breaks = mois_ids,
    label = mois_noms
  )

# Sauvegarde du graphique
ggsave("03_output/01_ventes_total_par_mois.png", width = 10, height = 8, dpi = 100)





##   TOTAL DE VENTES PAR Famille ET PAR MOIS 
print("Analyse du nombre total de ventes par mois par Familles de produits...")

# Manipulation des données
ventes_total_par_mois_par_Famille <- sourceData %>%
  group_by(mois_vente_id, Famille_produit) %>%
  count() %>%
  rename(nombre_ventes = n)

# Sauvegarde des données
write_csv(ventes_total_par_mois_par_Famille, "03_output/02_ventes_total_par_mois_par_Famille.csv")



# Création des graphiques

plot <- ggplot(data = ventes_total_par_mois_par_Famille, aes(x = mois_vente_id, y = nombre_ventes, fill=Famille_produit))+
  geom_bar(stat="identity")+
  labs(
    title = "Nombre total de ventes par mois par Familles",
    subtitle = "Données du dataset KaDo",
    x = "Mois",
    y = "Nombre de ventes",
    fill = "Familles de produits"
  )+
  scale_y_continuous(labels = scales::comma)
ggsave("03_output/02_01_ventes_total_par_mois_par_Famille.png", width = 10, height = 8, dpi = 100)


plot <- ggplot(data = ventes_total_par_mois_par_Famille, aes(x = mois_vente_id, y = nombre_ventes, fill=Famille_produit))+
  geom_bar(stat="identity", position="fill")+
  labs(
    title = "Répartition des ventes par mois par Familles",
    subtitle = "Données du dataset KaDo",
    x = "Mois",
    y = "Nombre de ventes",
    fill = "Familles de produits"
  )+
  scale_y_continuous(labels = scales::percent)
ggsave("03_output/02_02_repartition_ventes_par_mois_par_Famille.png", width = 10, height = 8, dpi = 100)

plot <- ggplot(data = ventes_total_par_mois_par_Famille, aes(x = mois_vente_id, y = nombre_ventes, fill=Famille_produit))+
  geom_bar(stat="identity", position="dodge")+
  labs(
    title = "Ventes par Familles par mois",
    subtitle = "Données du dataset KaDo",
    x = "Mois",
    y = "Nombre de ventes",
    fill = "Familles de produits"
  )+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(
    breaks = mois_ids,
    label = mois_noms
  )
ggsave("03_output/02_03_ventes_par_Familles_par_mois.png", width = 10, height = 8, dpi = 100)

plot <- ggplot(data = ventes_total_par_mois_par_Famille, aes(x = mois_vente_id, y = nombre_ventes, fill = Famille_produit))+
  geom_bar(stat="identity", position="dodge")+
  labs(
    title = "Ventes par mois pour chaque Famille",
    subtitle = "Données du dataset KaDo",
    x = "Mois",
    y = "Nombre de ventes",
    fill = "Familles de produits"
  )+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(
    breaks = mois_ids
  )+
  facet_wrap(~Famille_produit)
ggsave("03_output/02_04_ventes_par_mois_pour_chaque_Famille.png", width = 10, height = 8, dpi = 100)
