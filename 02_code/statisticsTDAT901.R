# INSTALL AND LOAD PACKAGES ################################
library(datasets)
library(tidyverse)
library(sparklyr)
library(readxl)
library(data.table)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio, sparklyr)
options(scipen = 100)

# Importing Dataset
kado_csv <-import("~/Projects/School/TDAT901/KaDo.csv")
head(kado_csv)

# Getting stats from the dataset
# Sales By Family
table(kado_csv$FAMILLE)
# Execute this to get better plots ↓
op <- par(mar = c(10,4,4,2) + 0.1)
barplot(table(kado_csv$FAMILLE),
        las = 2,
        main = "Quantité de vente par Famille")

# Sales by Univers
barplot(table(kado_csv$UNIVERS),
        las=2,
        main = "Quantité de produit vendu par Univers")

# Sales by Maille
barplot(table(kado_csv$MAILLE),
        las=2,
        main = "Quantité de produit vendu par Maille")

# Getting all the items of the dataset without clients information and prices of the items
# We deleted prices because for  the same item we have multiple prices
itemsDF = subset(kado_csv, select = -c(TICKET_ID, MOIS_VENTE, CLI_ID, PRIX_NET))
itemsDF <- unique(itemsDF)

barplot(table(itemsDF$FAMILLE),
        las=2,
        main= "Quantité de produits par famille",
        ylab = "Nombre de produits")

barplot(table(itemsDF$UNIVERS),
        las=2,
        main= "Quantité de produits par Univers",
        ylab = "Nombre de produits")

barplot(table(itemsDF$MAILLE),
        las=2,
        main= "Quantité de produits par Maille",
        ylab = "Nombre de produits")

################################################
#     Getting best selling items by Family     #
################################################

# Hygiene Family #
hygieneFam = table(kado_csv[kado_csv$FAMILLE=="HYGIENE", ]$LIBELLE)
bestHygieneItems = as.data.frame(hygieneFam[order(hygieneFam, decreasing = TRUE)])
colnames(bestHygieneItems)[1] <- "Libellé"
colnames(bestHygieneItems)[2] <- "NbDeVentes"

# Capillary Family #
capillaryFam = table(kado_csv[kado_csv$FAMILLE=="CAPILLAIRES", ]$LIBELLE)
bestCapillaryItems = as.data.frame(capillaryFam[order(capillaryFam, decreasing = TRUE)])
colnames(bestCapillaryItems)[1] <- "Libellé"
colnames(bestCapillaryItems)[2] <- "NbDeVentes"

# Makeup Family #
makeupFam = table(kado_csv[kado_csv$FAMILLE=="MAQUILLAGE", ]$LIBELLE)
bestMakeupItems = as.data.frame(makeupFam[order(makeupFam, decreasing = TRUE)])
colnames(bestMakeupItems)[1] <- "Libellé"
colnames(bestMakeupItems)[2] <- "NbDeVentes"

# Various Family #
variousFam = table(kado_csv[kado_csv$FAMILLE=="MULTI FAMILLES", ]$LIBELLE)
bestVariousItems = as.data.frame(variousFam[order(variousFam, decreasing = TRUE)])
colnames(bestVariousItems)[0] <- "Libellé"
colnames(bestVariousItems)[1] <- "NbDeVentes"

# Parfum Family #
parfumFam = table(kado_csv[kado_csv$FAMILLE=="PARFUMAGE", ]$LIBELLE)
bestParfumItems = as.data.frame(parfumFam[order(parfumFam, decreasing = TRUE)])
colnames(bestParfumItems)[1] <- "Libellé"
colnames(bestParfumItems)[2] <- "NbDeVentes"

# Natural Health Care Family #
natHealthFam = table(kado_csv[kado_csv$FAMILLE=="SANTE NATURELLE", ]$LIBELLE)
bestNatHealthItems = as.data.frame(natHealthFam[order(natHealthFam, decreasing = TRUE)])
colnames(bestNatHealthItems)[0] <- "Libellé"
colnames(bestNatHealthItems)[1] <- "NbDeVentes"

# Body Care Family #
bodyCareFam = table(kado_csv[kado_csv$FAMILLE=="SOINS DU CORPS", ]$LIBELLE)
bestBodyCareItems = as.data.frame(bodyCareFam[order(bodyCareFam, decreasing = TRUE)])
colnames(bestBodyCareItems)[1] <- "Libellé"
colnames(bestBodyCareItems)[2] <- "NbDeVentes"

# Face Care Family #
faceCareFam = table(kado_csv[kado_csv$FAMILLE=="SOINS DU VISAGE", ]$LIBELLE)
bestFaceCareItems = as.data.frame(faceCareFam[order(faceCareFam, decreasing = TRUE)])
colnames(bestFaceCareItems)[1] <- "Libellé"
colnames(bestFaceCareItems)[2] <- "NbDeVentes"

# Sun Family #
sunFam = table(kado_csv[kado_csv$FAMILLE=="SOLAIRES", ]$LIBELLE)
bestSunItems = as.data.frame(sunFam[order(sunFam, decreasing = TRUE)])
colnames(bestSunItems)[1] <- "Libellé"
colnames(bestSunItems)[2] <- "NbDeVentes"

##################################################
# Getting the mean price for items by categories #
##################################################
itemsWithPriceDF = subset(kado_csv, select = -c(TICKET_ID, MOIS_VENTE, CLI_ID))
itemsWithPriceDF <- unique(itemsWithPriceDF)


for (product in bestHygieneItems$Libellé)
{
  for(productWithPrice in itemsWithPriceDF){
    print(productWithPrice[PRIX_NET])
  }
}

#sort(table(variable),decreasing=TRUE)[1]















