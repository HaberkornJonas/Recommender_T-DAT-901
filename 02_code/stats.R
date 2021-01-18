## IMPORTS ###############################
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(Hmisc)
library(tidyr)

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

## PRE-PROCESSING ###############################
## Clearing Data, Removing those 2 Families because they are not relevant
sourceData <- as.data.frame(sourceData)
sourceData <- sourceData[!(sourceData$Famille=="MULTI FAMILLES"),]
sourceData <- sourceData[!(sourceData$Famille=="SANTE NATURELLE"),]


## Generating label to ID matching for families and products
Customers <- sourceData %>%
  select(ClientId)                                                             # Keep Famille column
write_csv(Customers, paste(projectPath, "03_output/Customers.csv", sep=""))    # Generating csv mapping

Familles <- sourceData %>%                                          
  select(Famille) %>%                                                          # Keep Famille column
  distinct() %>%                                                               # Keep only distinc familles
  mutate(FamilleId=row_number())                                               # Assign ID to famille
write_csv(Familles, paste(projectPath, "03_output/Familles.csv", sep=""))      # Generating csv mapping


Produits <- sourceData %>%                                          
  select(Libelle) %>%                                                          # Keep Libelle column
  distinct() %>%                                                               # Keep only distinc produit
  mutate(ProduitId=row_number())                                               # Assign ID to produit
write_csv(Produits, paste(projectPath, "03_output/Produits.csv", sep=""))      # Generating csv mapping




## STATS ###############################

####  [G] General habits and facts ####
####  [G_T] General tickets stats  ####
## [G_T_Y_T] ############################### 
print("[G_T_Y_T]...")

# Manipulation des donnees
G_T_Y_T <- sourceData$TicketId %>%                                             # Use Ticket column
           unique() %>%                                                        # Get unique values (# different tickets)
           length() %>%                                                        # Get the number of unique values
           data.frame()                                                        # Make it a dataframe
colnames(G_T_Y_T) <- c("n")                                                    # Give column name for CSV

# Sauvegarde des donnees
write_csv(G_T_Y_T, paste(projectPath, "03_output/G_T_Y_T.csv", sep=""))

# Creation du graphique
df <- data.frame(x = 1, y = G_T_Y_T[1,1])
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
ggsave(paste(projectPath, "03_output/G_T_Y_T.png", sep=""), width = 5, height = 8, dpi = 100)
sprintf("[G_T_Y_T] There was a total of %s tickets for this year.", G_T_Y_T[1,1])









## [G_T_M_T] ############################### 
print("[G_T_M_T]...")

# Manipulation des donnees
G_T_M_T <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G_T_M_T <- aggregate(G_T_M_T$TicketId, by=list(G_T_M_T$MoisVenteId), FUN=length) %>%   # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns

# Sauvegarde des donnees
write_csv(G_T_M_T, paste(projectPath, "03_output/G_T_M_T.csv", sep=""))

# Creation du graphique
plot <- ggplot(data = G_T_M_T, aes(x = as.numeric(MoisVenteId), y = n))+
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
ggsave(paste(projectPath, "03_output/G_T_M_T.png", sep=""), width = 12, height = 8, dpi = 100)
print("[G_T_M_T] Done!")











## [G_T_M_M] ############################### 
print("[G_T_M_M]...")

# Manipulation des donnees
G_T_M_M <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G_T_M_M <- aggregate(G_T_M_M$TicketId, by=list(G_T_M_M$MoisVenteId), FUN=length) %>%   # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns
G_T_M_M <- mean(G_T_M_M$n) %>%                                                         # Get mean value
           data.frame()                                                                # Make it a dataframe
colnames(G_T_M_M) <- c("mean")                                                         # Give column name for CSV

# Sauvegarde des donnees
write_csv(G_T_M_M, paste(projectPath, "03_output/G_T_M_M.csv", sep=""))

# Creation du graphique
df <- data.frame(x = 1, y = G_T_M_M[1,1])
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
ggsave(paste(projectPath, "03_output/G_T_M_M.png", sep=""), width = 5, height = 8, dpi = 100)
sprintf("[G_T_M_M] Done!")











## [G_T_M_S] ############################### 
print("[G_T_M_S]...")

# Manipulation des donnees
G_T_M_S <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G_T_M_S <- aggregate(G_T_M_S$TicketId, by=list(G_T_M_S$MoisVenteId), FUN=length) %>%   # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns
G_T_M_S <- sd(G_T_M_S$n) %>%                                                           # Get standard deviation
           data.frame()                                                                # Make it a dataframe
colnames(G_T_M_S) <- c("sd")                                                           # Give column name for CSV

# Sauvegarde des donnees
write_csv(G_T_M_S, paste(projectPath, "03_output/G_T_M_S.csv", sep=""))

# Creation du graphique
df <- data.frame(x = 1, y = G_T_M_S[1,1])
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
ggsave(paste(projectPath, "03_output/G_T_M_S.png", sep=""), width = 5, height = 8, dpi = 100)
sprintf("[G_T_M_S] Done!")












## [G_T_M] ############################### 
print("[G_T_M]...")

# Manipulation des donnees
mean <- G_T_M_M$mean
sd <- G_T_M_S$sd
G_T_M <- data.frame(mean, sd)

# Creation du graphique
df <- data.frame(x=1, y=G_T_M$mean)
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
ggsave(paste(projectPath, "03_output/G_T_M.png", sep=""), width = 7, height = 8, dpi = 100)
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
productsData <- as.data.frame(unique(productsData))
productsData[, "PrixNet"] <- NA

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
          title = "Vente sur l'annee par Famille"
        )

print(G_F_Y_T_Plot)

## [G_F_M_T] ############################### 
# TODO


## [G_F_M_M] ############################### 
# TODO


## [G_F_M_S] ############################### 
# TODO


## [G_F_P_M] ###############################
# Gettings all the sells for the products and removing useless columns
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
for (product in bestCapillaryItems$Libelle)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestCapillaryItems[bestCapillaryItems$Libellé==product,]$Prix = productFrequentPrice
  productsData[productsData$Libelle==product, ]$PrixNet = productFrequentPrice
}

# Applying a correct price for every product in the hygiene family
for (product in bestHygieneItems$Libelle)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestHygieneItems[bestHygieneItems$Libellé==product,]$Prix = productFrequentPrice
  productsData[productsData$Libelle==product, ]$PrixNet = productFrequentPrice
}

# Applying a correct price for every product in the makeup family
for (product in bestMakeupItems$Libelle)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestMakeupItems[bestMakeupItems$Libellé==product,]$Prix = productFrequentPrice
  productsData[productsData$Libelle==product, ]$PrixNet = productFrequentPrice
}

# Applying a correct price for every product in the parfume family
for (product in bestParfumItems$Libelle)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestParfumItems[bestParfumItems$Libellé==product,]$Prix = productFrequentPrice
  productsData[productsData$Libelle==product, ]$PrixNet = productFrequentPrice
}

# Applying a correct price for every product in the body care family
for (product in bestBodyCareItems$Libelle)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestBodyCareItems[bestBodyCareItems$Libellé==product,]$Prix = productFrequentPrice
  productsData[productsData$Libelle==product, ]$PrixNet = productFrequentPrice
}

# Applying a correct price for every product in the face care family
for (product in bestFaceCareItems$Libelle)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestFaceCareItems[bestFaceCareItems$Libellé==product,]$Prix = productFrequentPrice
  productsData[productsData$Libelle==product, ]$PrixNet = productFrequentPrice
}

# Applying a correct price for every product in the sun care family
for (product in bestSunItems$Libelle)
{
  productPrices <- productsWithPriceDF %>% filter(productsWithPriceDF$Libelle==product)
  productFrequentPrice = as.double(names(sort(table(productPrices$PrixNet), decreasing = TRUE)[1]))
  bestSunItems[bestSunItems$Libellé==product,]$Prix = productFrequentPrice
  productsData[productsData$Libelle==product, ]$PrixNet = productFrequentPrice
}

# Means of prices of items in each Family
mean(bestCapillaryItems[["PRIX"]])
mean(bestHygieneItems[["PRIX"]])
mean(bestMakeupItems[["PRIX"]])
mean(bestParfumItems[["PRIX"]])
mean(bestBodyCareItems[["PRIX"]])
mean(bestFaceCareItems[["PRIX"]])
mean(bestSunItems[["PRIX"]])

## [G_F_P_S] ############################### 
sd(bestCapillaryItems[["PRIX"]])
sd(bestHygieneItems[["PRIX"]])
sd(bestMakeupItems[["PRIX"]])
sd(bestParfumItems[["PRIX"]])
sd(bestBodyCareItems[["PRIX"]])
sd(bestFaceCareItems[["PRIX"]])
sd(bestSunItems[["PRIX"]])


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
salesByUniversYear= as.data.frame(table(sourceData$Univers))
colnames(salesByUniversYear)[1] = "Univers"
colnames(salesByUniversYear)[2] = "Ventes"

salesByUniversYear_Plot <- ggplot(salesByUniversYear,
                                  aes(x=Univers, y=Ventes)) +
  geom_bar(stat="identity") +
  labs(
    title = "Ventes par Univers durant l'année"
  ) + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(salesByUniversYear_Plot)

## [G_U_M_T] ############################### 
# TODO


## [G_U_M_M] ############################### 
# TODO


## [G_U_M_S] ############################### 
# TODO


## [G_U_P_M] ############################### 
universDf = as.data.frame(table(productsData$Univers))
universDf = subset(universDf, select = -c(Freq))
colnames(universDf)[1]="Univers"
universDf[, "PrixMoyen"] <- NA

for(univers in universDf$Univers){
  universDf[universDf$Univers==univers, ]$PrixMoyen = mean(productsData[productsData$Univers==univers,]$PrixNet)
}

meanPriceByUnivers_Plot <- ggplot(universDf,
                                  aes(x=Univers, y=PrixMoyen)) +
  geom_bar(stat="identity") +
  labs(
    title = "Prix Moyen par Univers"
  ) + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(meanPriceByUnivers_Plot)
## [G_U_P_S] ############################### 
sd(universDf[["PrixMoyen"]])


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
salesByMaillesYear = as.data.frame(table(sourceData$Maille))
colnames(salesByMaillesYear)[1] = "Maille"
colnames(salesByMaillesYear)[2] = "Ventes"

salesByMaillesYear_Plot <- ggplot(salesByMaillesYear,
                                aes(x = Maille, y=Ventes)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Vente par Maille durant l'année"
  ) + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(salesByMaillesYear_Plot)

## [G_M_M_T] ############################### 
# TODO


## [G_M_M_M] ############################### 
# TODO


## [G_M_M_S] ############################### 
# TODO


## [G_M_P_M] ############################### 
maillesDf = as.data.frame(table(productsData$Maille))
maillesDf = subset(maillesDf, select = -c(Freq))
colnames(maillesDf)[1]="Maille"
maillesDf[, "PrixMoyen"] <- NA

for(maille in maillesDf$Maille){
  maillesDf[maillesDf$Maille==maille, ]$PrixMoyen = mean(productsData[productsData$Maille==maille,]$PrixNet)
}

meanPriceByMaille_Plot <- ggplot(maillesDf,
                                  aes(x=Maille, y=PrixMoyen)) +
  geom_bar(stat="identity") +
  labs(
    title = "Prix Moyen par Maille"
  ) + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(meanPriceByMaille_Plot)
## [G_M_P_S] ############################### 
sd(maillesDf[["PrixMoyen"]])


####  [G_MF] General Most favorites Products ####
## [G_MF_Y] ############################### 
salesByProductYear = as.data.frame(sort(table(sourceData$Libelle), decreasing = TRUE))
colnames(salesByProductYear)[1] = "Produit"
colnames(salesByProductYear)[2] = "Ventes"

salesByProductYear = head(salesByProductYear, 50)

salesByProductYear_Plot <- ggplot(salesByProductYear,
                                 aes(x=Produit, y=Ventes)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Vente par produit sur l'année"
  ) + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(salesByProductYear_Plot)
## [G_MF_M] ############################### 
# TODO


## [G_MF_F] ############################### 
################################################
#     Getting best selling items by Family     #
################################################
# Hygiene Family #
hygieneFam = table(sourceData[sourceData$Famille=="HYGIENE", ]$Libelle)
bestHygieneItems = as.data.frame(hygieneFam[order(hygieneFam, decreasing = TRUE)])
colnames(bestHygieneItems)[1] <- "Libelle"
colnames(bestHygieneItems)[2] <- "Ventes"
write_csv(bestHygieneItems, paste(projectPath, "03_output/bestHygieneItems.csv", sep=""))

# Capillary Family #
capillaryFam = table(sourceData[sourceData$Famille=="CAPILLAIRES", ]$Libelle)
bestCapillaryItems = as.data.frame(capillaryFam[order(capillaryFam, decreasing = TRUE)])
colnames(bestCapillaryItems)[1] <- "Libelle"
colnames(bestCapillaryItems)[2] <- "Ventes"
write_csv(bestCapillaryItems, paste(projectPath, "03_output/bestCapillaryItems.csv", sep=""))

# Makeup Family #
makeupFam = table(sourceData[sourceData$Famille=="MAQUILLAGE", ]$Libelle)
bestMakeupItems = as.data.frame(makeupFam[order(makeupFam, decreasing = TRUE)])
colnames(bestMakeupItems)[1] <- "Libelle"
colnames(bestMakeupItems)[2] <- "Ventes"
write_csv(bestMakeupItems, paste(projectPath, "03_output/bestMakeupItems.csv", sep=""))

# Parfum Family #
parfumFam = table(sourceData[sourceData$Famille=="PARFUMAGE", ]$Libelle)
bestParfumItems = as.data.frame(parfumFam[order(parfumFam, decreasing = TRUE)])
colnames(bestParfumItems)[1] <- "Libelle"
colnames(bestParfumItems)[2] <- "Ventes"
write_csv(bestParfumItems, paste(projectPath, "03_output/bestParfumItems.csv", sep=""))

# Body Care Family #
bodyCareFam = table(sourceData[sourceData$Famille=="SOINS DU CORPS", ]$Libelle)
bestBodyCareItems = as.data.frame(bodyCareFam[order(bodyCareFam, decreasing = TRUE)])
colnames(bestBodyCareItems)[1] <- "Libelle"
colnames(bestBodyCareItems)[2] <- "Ventes"
write_csv(bestBodyCareItems, paste(projectPath, "03_output/bestBodyCareItems.csv", sep=""))

# Face Care Family #
faceCareFam = table(sourceData[sourceData$Famille=="SOINS DU VISAGE", ]$Libelle)
bestFaceCareItems = as.data.frame(faceCareFam[order(faceCareFam, decreasing = TRUE)])
colnames(bestFaceCareItems)[1] <- "Libelle"
colnames(bestFaceCareItems)[2] <- "Ventes"
write_csv(bestFaceCareItems, paste(projectPath, "03_output/bestFaceCareItems.csv", sep=""))

# Sun Family #
sunFam = table(sourceData[sourceData$Famille=="SOLAIRES", ]$Libelle)
bestSunItems = as.data.frame(sunFam[order(sunFam, decreasing = TRUE)])
colnames(bestSunItems)[1] <- "Libelle"
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
print("[C_T_Y_T]...")

# Manipulation des donnees
C_T_Y_T <- sourceData %>%                                          
  select(TicketId, ClientId) %>%                                               # Keep TicketId and ClientId columns
  group_by(ClientId) %>%                                                       # Group by ClientId
  distinct() %>%                                                               # Keep only distict values (clientID, TicketId couple)
  tally()                                                                      # Make a count of the number of different tickets per customer

# Sauvegarde des donnees
write_csv(C_T_Y_T, paste(projectPath, "03_output/C_T_Y_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[G_T_Y_T] Done!")




## [C_T_M_T] ############################### 
print("[C_T_M_T]...")

# Manipulation des donnees
C_T_M_T <- sourceData %>%                                          
  select(TicketId, ClientId, MoisVenteId) %>%                                  # Keep TicketId, ClientId and MoisVenteId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  distinct() %>%                                                               # Keep only distict values (clientID, MoisVenteId, TicketId couple)
  tally()                                                                      # Make a count of the number of different tickets per customer per month

# Sauvegarde des donnees
write_csv(C_T_M_T, paste(projectPath, "03_output/C_T_M_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_T_M_T] Done!")




## [C_T_M_M] ############################### 
print("[C_T_M_M]...")

# Manipulation des donnees
C_T_M_M <- sourceData %>%                                          
  select(TicketId, ClientId, MoisVenteId) %>%                                  # Keep TicketId, ClientId and MoisVenteId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  distinct() %>%                                                               # Keep only distict values (clientID, MoisVenteId, TicketId couple)
  tally() %>%                                                                  # Make a count of the number of different tickets per customer per month
  summarise(mean=round(mean(c(rep(0, 12-length(n)), n)), digits=2))            # Calculate mean number of tickets per month for each customer

# Sauvegarde des donnees
write_csv(C_T_M_M, paste(projectPath, "03_output/C_T_M_M.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_T_M_M] Done!")




## [C_T_M_S] ############################### 
print("[C_T_M_S]...")

# Manipulation des donnees
C_T_M_S <- sourceData %>%                                          
  select(TicketId, ClientId, MoisVenteId) %>%                                  # Keep TicketId, ClientId and MoisVenteId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  distinct() %>%                                                               # Keep only distict values (clientID, MoisVenteId, TicketId couple)
  tally() %>%                                                                  # Make a count of the number of different tickets per customer per month
  summarise(sd=round(sd(c(rep(0, 12-length(n)), n)), digits=2))                # Calculate mean number of tickets per month for each customer


# Sauvegarde des donnees
write_csv(C_T_M_S, paste(projectPath, "03_output/C_T_M_S.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_T_M_S] Done!")








####  [C_S] Customer spending stats  ####
## [C_S_Y_T] ############################### 
print("[C_S_Y_T]...")

# Manipulation des donnees
C_S_Y_T <- sourceData %>%                                          
  select(ClientId, PrixNet) %>%                                                # Keep PrixNet and ClientId columns
  group_by(ClientId) %>%                                                       # Group by ClientId
  summarise(n=round(sum(PrixNet), digits=2))                                   # Calculate spendings of the year
  
# Sauvegarde des donnees
write_csv(C_S_Y_T, paste(projectPath, "03_output/C_S_Y_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_Y_T] Done!")




## [C_S_M_T] ############################### 
print("[C_S_M_T]...")

# Manipulation des donnees
C_S_M_T <- sourceData %>%                                          
  select(ClientId, MoisVenteId, PrixNet) %>%                                  # Keep PrixNet, MoisVenteId and ClientId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  summarise(n=round(sum(PrixNet), digits=2))                                   # Calculate spendings of each month per customer

# Sauvegarde des donnees
write_csv(C_S_M_T, paste(projectPath, "03_output/C_S_M_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_M_T] Done!")




## [C_S_M_M] ############################### 
print("[C_S_M_M]...")

# Manipulation des donnees
C_S_M_M <- sourceData %>%                                          
  select(ClientId, MoisVenteId, PrixNet) %>%                                   # Keep PrixNet, MoisVenteId and ClientId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  summarise(n=round(sum(PrixNet), digits=2)) %>%                               # Calculate spendings of each month per customer
  summarise(mean=round(mean(c(rep(0, 12-length(n)), n)), digits=2))            # Calculate mean spendings per month per customer

# Sauvegarde des donnees
write_csv(C_S_M_M, paste(projectPath, "03_output/C_S_M_M.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_M_M] Done!")




## [C_S_M_S] ############################### 
print("[C_S_M_S]...")

# Manipulation des donnees
C_S_M_S <- sourceData %>%                                          
  select(ClientId, MoisVenteId, PrixNet) %>%                                   # Keep PrixNet, MoisVenteId and ClientId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  summarise(n=round(sum(PrixNet), digits=2)) %>%                               # Calculate spendings of each month per customer
  summarise(sd=round(sd(c(rep(0, 12-length(n)), n)), digits=2))                # Calculate standard deviation of spendings per month per customer

# Sauvegarde des donnees
write_csv(C_S_M_S, paste(projectPath, "03_output/C_S_M_S.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_M_S] Done!")




## [C_S_T_M] ############################### 
print("[C_S_T_M]...")

# Manipulation des donnees
C_S_T_M <- sourceData %>%                                          
  select(ClientId, TicketId, PrixNet) %>%                                      # Keep TicketId, PrixNet and ClientId columns
  group_by(ClientId, TicketId) %>%                                             # Group by ClientId and TicketId
  summarise(n=round(sum(PrixNet), digits=2)) %>%                               # Calculate spendings of each tickets
  group_by(ClientId) %>%                                                       # Group by ClientId
  summarise(mean=round(mean(n), digits=2))                                     # Calculate mean spendings per ticket per customer
  
# Sauvegarde des donnees
write_csv(C_S_T_M, paste(projectPath, "03_output/C_S_T_M.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_T_M] Done!")




## [C_S_T_S] ############################### 
print("[C_S_T_S]...")

# Manipulation des donnees
C_S_T_S <- sourceData %>%                                          
  select(ClientId, TicketId, PrixNet) %>%                                      # Keep TicketId, PrixNet and ClientId columns
  group_by(ClientId, TicketId) %>%                                             # Group by ClientId and TicketId
  summarise(n=round(sum(PrixNet), digits=2)) %>%                               # Calculate spendings of each tickets
  group_by(ClientId) %>%                                                       # Group by ClientId
  summarise(sd=round(sd(n), digits=2))                                         # Calculate standard deviation of spendings per ticket per customer
C_S_T_S[is.na(C_S_T_S)] <- 0                                                   # Replacing NA values with 0 for customers having only one ticket

# Sauvegarde des donnees
write_csv(C_S_T_S, paste(projectPath, "03_output/C_S_T_S.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_T_S] Done!")




## [C_S_P_M] ############################### 
print("[C_S_P_M]...")

# Manipulation des donnees
C_S_P_M <- sourceData %>%                                          
  select(ClientId, PrixNet) %>%                                                # Keep PrixNet and ClientId columns
  group_by(ClientId) %>%                                                       # Group by ClientId
  summarise(mean=round(mean(PrixNet), digits=2))                               # Calculate mean spendings per product per customer

# Sauvegarde des donnees
write_csv(C_S_P_M, paste(projectPath, "03_output/C_S_P_M.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_P_M] Done!")




## [C_S_P_S] ############################### 
print("[C_S_P_S]...")

# Manipulation des donnees
C_S_P_S <- sourceData %>%                                          
  select(ClientId, PrixNet) %>%                                                # Keep PrixNet and ClientId columns
  group_by(ClientId) %>%                                                       # Group by ClientId
  summarise(sd=round(sd(PrixNet), digits=2))                                   # Calculate mean spendings per product per customer
C_S_P_S[is.na(C_S_P_S)] <- 0                                                   # Replacing NA values with 0 for customers that bought only one product

# Sauvegarde des donnees
write_csv(C_S_P_S, paste(projectPath, "03_output/C_S_P_S.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_S_P_S] Done!")








####  [C_P] Customer Products bought stats  ####
## [C_P_Y_T] ############################### 
print("[C_P_Y_T]...")

# Manipulation des donnees
C_P_Y_T <- sourceData %>%                                          
  select(ClientId, PrixNet) %>%                                                # Keep PrixNet and ClientId columns
  group_by(ClientId) %>%                                                       # Group by ClientId
  tally()                                                                      # Make a count of the number of products per customer

# Sauvegarde des donnees
write_csv(C_P_Y_T, paste(projectPath, "03_output/C_P_Y_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_P_Y_T] Done!")





## [C_P_M_T] ############################### 
print("[C_P_M_T]...")

# Manipulation des donnees
C_P_M_T <- sourceData %>%                                          
  select(ClientId, MoisVenteId, PrixNet) %>%                                   # Keep PrixNet, MoisVenteId and ClientId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  tally()                                                                      # Make a count of the number of products per customer per month

# Sauvegarde des donnees
write_csv(C_P_M_T, paste(projectPath, "03_output/C_P_M_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_P_M_T] Done!")




## [C_P_M_M] ############################### 
print("[C_P_M_M]...")

# Manipulation des donnees
C_P_M_M <- sourceData %>%                                          
  select(ClientId, MoisVenteId, PrixNet) %>%                                   # Keep PrixNet, MoisVenteId and ClientId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  tally() %>%                                                                  # Make a count of the number of products per customer per month
  summarise(mean=round(mean(c(rep(0, 12-length(n)), n)), digits=2))            # Calculate mean number of products per month for each customer

# Sauvegarde des donnees
write_csv(C_P_M_M, paste(projectPath, "03_output/C_P_M_M.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_P_M_M] Done!")




## [C_P_M_S] ############################### 
print("[C_P_M_S]...")

# Manipulation des donnees
C_P_M_S <- sourceData %>%                                          
  select(ClientId, MoisVenteId, PrixNet) %>%                                   # Keep PrixNet, MoisVenteId and ClientId columns
  group_by(ClientId, MoisVenteId) %>%                                          # Group by ClientId and MoisVenteId
  tally() %>%                                                                  # Make a count of the number of products per customer per month
  summarise(sd=round(sd(c(rep(0, 12-length(n)), n)), digits=2))                # Calculate standard deviation of products per month for each customer

# Sauvegarde des donnees
write_csv(C_P_M_S, paste(projectPath, "03_output/C_P_M_S.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_P_M_S] Done!")






####  [C_C] Customer characteristics  ####
## [C_C_G] ############################### 
# TODO (Bonus)






####  [C_F] Customer product families stats  ####
## [C_F_Y_T] ############################### 
print("[C_F_Y_T]...")

# Manipulation des donnees
C_F_Y_T <- sourceData %>%                                          
  select(ClientId, Famille) %>%                                                # Keep ClientId and Famille columns
  group_by(ClientId, Famille) %>%                                              # Group by ClientId and Famille
  tally() %>%                                                                  # Make a count of the number of products per family per customer
  merge(Familles, by="Famille", sort=F) %>%                                    # Matching Famille to its FamilleId
  subset(select= -c(Famille)) %>%                                              # Removing Famille column to save memory and only use the assigned FamilleId
  arrange(ClientId, FamilleId)


# Sauvegarde des donnees
write_csv(C_F_Y_T, paste(projectPath, "03_output/C_F_Y_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_F_Y_T] Done!")




## [C_F_M_T] ############################### 
print("[C_F_M_T]...")

# Manipulation des donnees
C_F_M_T <- sourceData %>%                                          
  select(ClientId, MoisVenteId, Famille) %>%                                   # Keep ClientId, Famille and MoisVenteId columns
  group_by(ClientId, MoisVenteId, Famille) %>%                                 # Group by ClientId, MoisVenteId and Famille
  tally() %>%                                                                  # Make a count of the number of products per family per month per customer
  merge(Familles, by="Famille", sort=F) %>%                                    # Matching Famille to its FamilleId
  subset(select= -c(Famille))                                                  # Removing Famille column to save memory and only use the assigned FamilleId

# Sauvegarde des donnees
write_csv(C_F_M_T, paste(projectPath, "03_output/C_F_M_T.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_F_M_T] Done!")




## [C_F_M_M] ############################### 
print("[C_F_M_M]...")

# Manipulation des donnees
C_F_M_M <- sourceData %>%                                          
  select(ClientId, Famille, MoisVenteId) %>%                                   # Keep ClientId, Famille and MoisVenteId columns
  group_by(ClientId, Famille, MoisVenteId) %>%                                 # Group by ClientId, MoisVenteId and Famille
  tally() %>%                                                                  # Make a count of the number of products per family per month per customer
  group_by(ClientId, Famille) %>%                                              # Group by ClientId, MoisVenteId and Famille
  summarise(mean=round(mean(c(rep(0, 12-length(n)), n)), digits=2)) %>%        # Calculate mean number of product bouught per famille and per month for each customer
  merge(Familles, by="Famille", sort=F) %>%                                    # Matching Famille to its FamilleId
  subset(select= -c(Famille))                                                  # Removing Famille column to save memory and only use the assigned FamilleId
  
# Sauvegarde des donnees
write_csv(C_F_M_M, paste(projectPath, "03_output/C_F_M_M.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_F_M_M] Done!")




## [C_F_M_S] ############################### 
print("[C_F_M_S]...")

# Manipulation des donnees
C_F_M_S <- sourceData %>%                                          
  select(ClientId, Famille, MoisVenteId) %>%                                   # Keep ClientId, Famille and MoisVenteId columns
  group_by(ClientId, Famille, MoisVenteId) %>%                                 # Group by ClientId, MoisVenteId and Famille
  tally() %>%                                                                  # Make a count of the number of products per family per month per customer
  group_by(ClientId, Famille) %>%                                              # Group by ClientId, MoisVenteId and Famille
  summarise(sd=round(sd(c(rep(0, 12-length(n)), n)), digits=2)) %>%            # Calculate standard deviation of product bought per famille and per month for each customer
  merge(Familles, by="Famille", sort=F) %>%                                    # Matching Famille to its FamilleId
  subset(select= -c(Famille))                                                  # Removing Famille column to save memory and only use the assigned FamilleId

# Sauvegarde des donnees
write_csv(C_F_M_S, paste(projectPath, "03_output/C_F_M_S.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_F_M_S] Done!")





## [C_F_Y_MF] ############################### 
print("[C_F_Y_MF]...")

# Manipulation des donnees
C_F_Y_MF <- sourceData %>%                                          
  select(ClientId, Famille) %>%                                                # Keep ClientId and Famille columns
  group_by(ClientId, Famille) %>%                                              # Group by ClientId and Famille
  tally() %>%                                                                  # Make a count of the number of products per family per customer
  group_by(ClientId) %>%                                                       # Grouping by Client
  slice_max(n=1, order_by=n, with_ties=F) %>%                                  # Keeping only the most bought product familly
  merge(Familles, by="Famille", sort=F) %>%                                    # Matching Famille to its FamilleId
  subset(select= -c(Famille))                                                  # Removing Famille column to save memory and only use the assigned FamilleId

# Sauvegarde des donnees
write_csv(C_F_Y_MF, paste(projectPath, "03_output/C_F_Y_MF.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_F_Y_MF] Done!")





####  [C_F_S] Customer spendings per product families stats  ####
## [C_F_S_Y_M] ############################### 
# TODO


## [C_F_S_Y_S] ############################### 
# TODO






####  [C_F_MF] Customer most favorite product per families stats  ####
## [C_F_MF_Y] ############################### 
print("[C_F_MF_Y]...")

# Manipulation des donnees
C_F_MF_Y <- sourceData %>%                                          
  select(ClientId, Famille, Libelle) %>%                                       # Keep ClientId, Famille and Libelle columns
  group_by(ClientId, Famille, Libelle) %>%                                     # Group by ClientId, Famille and Libelle
  tally() %>%                                                                  # Make a count of the number of time each product has been bought per customer
  group_by(ClientId, Famille) %>%                                              # Grouping by Client and Famille
  slice_max(n=3, order_by=n, with_ties=F) %>%                                  # Keeping only the top 3 most bought items
  merge(Produits, by="Libelle", sort=F) %>%                                    # Matching Libelle to its ProductId
  subset(select= -c(Libelle)) %>%                                              # Removing Libelle column to save memory and only use the assigned ProductId
  merge(Familles, by="Famille", sort=F) %>%                                    # Matching Famille to its FamilleId
  subset(select= -c(Famille)) %>%                                              # Removing Famille column to save memory and only use the assigned FamilleId
  arrange(ClientId, FamilleId, n)                                              # Arranging dataframe by ClientId, FamilleId, number of time product has been bought

# Sauvegarde des donnees
write_csv(C_F_MF_Y, paste(projectPath, "03_output/C_F_MF_Y.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_F_MF_Y] Done!")




####  [C_F_LF] Customer least favorite product per families stats  ####
## [C_F_LF_Y] ############################### 
# TODO (Not possible)






####  [C_MF] Customer most favorite product stats  ####
## [C_MF_Y] ############################### 
print("[C_MF_Y]...")

# Manipulation des donnees
C_MF_Y <- sourceData %>%                                          
  select(ClientId, Libelle) %>%                                                # Keep ClientId and Libelle columns
  group_by(ClientId, Libelle) %>%                                              # Group by ClientId and Libelle
  tally() %>%                                                                  # Make a count of the number of time each product has been bought per customer
  group_by(ClientId) %>%                                                       # Grouping by Client
  slice_max(n=3, order_by=n, with_ties=F) %>%                                  # Keeping only the top 3 most bought items
  merge(Produits, by="Libelle", sort=F) %>%                                    # Matching Libelle to its ProductId
  subset(select= -c(Libelle)) %>%                                              # Removing Libelle column to save memory and only use the assigned ProductId
  arrange(ClientId, n)                                                         # Arranging dataframe by ClientId and number of time product has been bought

# Sauvegarde des donnees
write_csv(C_MF_Y, paste(projectPath, "03_output/C_MF_Y.csv", sep=""))

# Pas de graphique (trop d'entrees, resultat illisible et long a generer)
sprintf("[C_MF_Y] Done!")




## [C_MF_M] ############################### 
# TODO (Needed ?)






####  [C_LF] Customer least favorite product stats  ####
## [C_LF_Y] ############################### 
# TODO (Not possible)
## [C_LF_M] ############################### 
# TODO (Not possible)

















## OLD STATS ####

if(FALSE){
  ## TOTAL DE VENTES PAR MOIS 
  print("Analyse du nombre total de ventes par mois...")
  
  # Manipulation des donnees
  ventes_total_par_mois <- sourceData %>%
    group_by(mois_vente_id) %>%
    count() %>%
    rename(nombre_ventes = n)
  
  # Sauvegarde des donnees
  write_csv(ventes_total_par_mois, "03_output/01_ventes_total_par_mois.csv")
  
  # Creation du graphique
  plot <- ggplot(data = ventes_total_par_mois, aes(x = mois_vente_id, y = nombre_ventes))+
    geom_bar(stat="identity", fill="steelblue3", color="steelblue3")+
    labs(
      title = "Nombre total de ventes par mois",
      subtitle = "Donnees du dataset KaDo",
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
  
  
  
  
  
  ##   TOTAL DE VENTES PAR FAMILLE ET PAR MOIS 
  print("Analyse du nombre total de ventes par mois par familles de produits...")
  
  # Manipulation des donnees
  ventes_total_par_mois_par_famille <- sourceData %>%
    group_by(mois_vente_id, famille_produit) %>%
    count() %>%
    rename(nombre_ventes = n)
  
  # Sauvegarde des donnees
  write_csv(ventes_total_par_mois_par_famille, "03_output/02_ventes_total_par_mois_par_famille.csv")
  
  
  
  # Creation des graphiques
  
  plot <- ggplot(data = ventes_total_par_mois_par_famille, aes(x = mois_vente_id, y = nombre_ventes, fill=famille_produit))+
    geom_bar(stat="identity")+
    labs(
      title = "Nombre total de ventes par mois par familles",
      subtitle = "Donnees du dataset KaDo",
      x = "Mois",
      y = "Nombre de ventes",
      fill = "Familles de produits"
    )+
    scale_y_continuous(labels = scales::comma)
  ggsave("03_output/02_01_ventes_total_par_mois_par_famille.png", width = 10, height = 8, dpi = 100)
  
  
  plot <- ggplot(data = ventes_total_par_mois_par_famille, aes(x = mois_vente_id, y = nombre_ventes, fill=famille_produit))+
    geom_bar(stat="identity", position="fill")+
    labs(
      title = "Repartition des ventes par mois par familles",
      subtitle = "Donnees du dataset KaDo",
      x = "Mois",
      y = "Nombre de ventes",
      fill = "Familles de produits"
    )+
    scale_y_continuous(labels = scales::percent)
  ggsave("03_output/02_02_repartition_ventes_par_mois_par_famille.png", width = 10, height = 8, dpi = 100)
  
  plot <- ggplot(data = ventes_total_par_mois_par_famille, aes(x = mois_vente_id, y = nombre_ventes, fill=famille_produit))+
    geom_bar(stat="identity", position="dodge")+
    labs(
      title = "Ventes par familles par mois",
      subtitle = "Donnees du dataset KaDo",
      x = "Mois",
      y = "Nombre de ventes",
      fill = "Familles de produits"
    )+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(
      breaks = mois_ids,
      label = mois_noms
    )
  ggsave("03_output/02_03_ventes_par_familles_par_mois.png", width = 10, height = 8, dpi = 100)
  
  plot <- ggplot(data = ventes_total_par_mois_par_famille, aes(x = mois_vente_id, y = nombre_ventes, fill = famille_produit))+
    geom_bar(stat="identity", position="dodge")+
    labs(
      title = "Ventes par mois pour chaque famille",
      subtitle = "Donnees du dataset KaDo",
      x = "Mois",
      y = "Nombre de ventes",
      fill = "Familles de produits"
    )+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(
      breaks = mois_ids
    )+
    facet_wrap(~famille_produit)
  ggsave("03_output/02_04_ventes_par_mois_pour_chaque_famille.png", width = 10, height = 8, dpi = 100)
}
