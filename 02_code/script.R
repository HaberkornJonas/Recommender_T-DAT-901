library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

## INFO: 
##    - ggplot tuto: https://r4ds.had.co.nz/data-visualisation.html

# valeures statiques
mois_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
mois_noms <- c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
mois_dictionary <- data.frame(mois_vente_id=c(1:12), mois_vente=mois_noms)

print("Lecture des donées...")
# import dataframe
sourceData <- read_csv("01_data/KaDo.csv") %>%
              rename(
                  ticket_id = TICKET_ID,
                  mois_vente_id = MOIS_VENTE,
                  prix_net = PRIX_NET,
                  famille_produit = FAMILLE,
                  univers_produit = UNIVERS,
                  maille_produit = MAILLE,
                  libelle_produit = LIBELLE,
                  client_id = CLI_ID
              )
print("Lecture des donées...")



################################
##   TOTAL DE VENTES PAR MOIS 
################################
print("Analyse du nombre total de ventes par mois...")

# Manipulation des données
ventes_total_par_mois <- sourceData %>%
  group_by(mois_vente_id) %>%
  count() %>%
  rename(nombre_ventes = n)

# Sauvegarde des données
write_csv(ventes_total_par_mois, "03_output/01_ventes_total_par_mois.csv")

# Création du graphique
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





################################################
##   TOTAL DE VENTES PAR FAMILLE ET PAR MOIS 
################################################
print("Analyse du nombre total de ventes par mois par familles de produits...")

# Manipulation des données
ventes_total_par_mois_par_famille <- sourceData %>%
  group_by(mois_vente_id, famille_produit) %>%
  count() %>%
  rename(nombre_ventes = n)

# Sauvegarde des données
write_csv(ventes_total_par_mois_par_famille, "03_output/02_ventes_total_par_mois_par_famille.csv")



# Création des graphiques

plot <- ggplot(data = ventes_total_par_mois_par_famille, aes(x = mois_vente_id, y = nombre_ventes, fill=famille_produit))+
  geom_bar(stat="identity")+
  labs(
    title = "Nombre total de ventes par mois par familles",
    subtitle = "Données du dataset KaDo",
    x = "Mois",
    y = "Nombre de ventes",
    fill = "Familles de produits"
  )+
  scale_y_continuous(labels = scales::comma)
ggsave("03_output/02_01_ventes_total_par_mois_par_famille.png", width = 10, height = 8, dpi = 100)


plot <- ggplot(data = ventes_total_par_mois_par_famille, aes(x = mois_vente_id, y = nombre_ventes, fill=famille_produit))+
  geom_bar(stat="identity", position="fill")+
  labs(
    title = "Répartition des ventes par mois par familles",
    subtitle = "Données du dataset KaDo",
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
ggsave("03_output/02_03_ventes_par_familles_par_mois.png", width = 10, height = 8, dpi = 100)

plot <- ggplot(data = ventes_total_par_mois_par_famille, aes(x = mois_vente_id, y = nombre_ventes, fill = famille_produit))+
  geom_bar(stat="identity", position="dodge")+
  labs(
    title = "Ventes par mois pour chaque famille",
    subtitle = "Données du dataset KaDo",
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





