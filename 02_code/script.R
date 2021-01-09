## IMPORTS ###############################
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

## INFO: 
##    - ggplot tuto: https://r4ds.had.co.nz/data-visualisation.html


## STATIC VALUES ###############################
moisIds <- c(1:12)
moisNoms <- c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
moisDictionary <- data.frame(moisVenteId=c(1:12), moisVente=moisNoms)


## READING DATASET ###############################
print("Lecture des donees...")

sourceData <- read_csv("C:/Users/jonas/Desktop/T-DAT/01_data/KaDo.csv") %>%
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





## [G_T_Y_T] ############################### 
print("[G_T_Y_T]...")

# Manipulation des donnees
G.T.Y.T <- sourceData$TicketId %>%                                          # Use Ticket column
           unique() %>%                                                     # Get unique values (# different tickets)
           length() %>%                                                     # Get the number of unique values
           data.frame()                                                     # Make it a dataframe
colnames(G.T.Y.T) <- c("n")                                                 # Give column name for CSV

# Sauvegarde des donnees
write_csv(G.T.Y.T, "C:/Users/jonas/Desktop/T-DAT/03_output/G.T.Y.T.csv")

# Creation du graphique
df <- data.frame(x = 1, y = G.T.Y.T[1,1])
plot <- ggplot(df, aes(x=x,y=y))+
  geom_segment( aes(x=x, xend=x, y=0, yend=y), size=1.3, alpha=0.9) +
  labs(
    title = "Nombre total de tickets",
    subtitle = "Donnees du dataset KaDo",
    x = "Ann�e",
    y = "Nombre de tickets"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Sauvegarde du graphique
ggsave("C:/Users/jonas/Desktop/T-DAT/03_output/G.T.Y.T.png", width = 5, height = 8, dpi = 100)
sprintf("[G_T_Y_T] There was a total of %s tickets for this year.", G.T.Y.T[1,1])









## [G_T_M_T] ############################### 
print("[G_T_M_T]...")

# Manipulation des donnees
G.T.M.T <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G.T.M.T <- aggregate(G.T.M.T$TicketId, by=list(G.T.M$MoisVenteId), FUN=length) %>%     # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns

# Sauvegarde des donnees
write_csv(G.T.M.T, "C:/Users/jonas/Desktop/T-DAT/03_output/G.T.M.T.csv")

# Creation du graphique
plot <- ggplot(data = G.T.M.T, aes(x = as.numeric(MoisVenteId), y = n))+
  geom_histogram(stat="identity", fill="steelblue3", color="steelblue3")+
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

# Sauvegarde du graphique
ggsave("C:/Users/jonas/Desktop/T-DAT/03_output/G.T.M.T.png", width = 12, height = 8, dpi = 100)
print("[G_T_M_T] Done!")











## [G_T_M_M] ############################### 
print("[G_T_M_M]...")

# Manipulation des donnees
G.T.M.M <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G.T.M.M <- aggregate(G.T.M.M$TicketId, by=list(G.T.M$MoisVenteId), FUN=length) %>%     # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns
G.T.M.M <- mean(G.T.M.M$n) %>%                                                         # Get mean value
           data.frame()                                                                # Make it a dataframe
colnames(G.T.M.M) <- c("mean")                                                         # Give column name for CSV

# Sauvegarde des donnees
write_csv(G.T.M.M, "C:/Users/jonas/Desktop/T-DAT/03_output/G.T.M.M.csv")

# Creation du graphique
df <- data.frame(x = 1, y = G.T.M.M[1,1])
plot <- ggplot(df, aes(x=x,y=y))+
  geom_segment( aes(x=x, xend=x, y=0, yend=y), size=1.3, alpha=0.9) +
  labs(
    title = "Nombre moyen de tickets par mois",
    subtitle = "Donnees du dataset KaDo",
    x = "Ann�e",
    y = "Nombre moyen de tickets par mois"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Sauvegarde du graphique
ggsave("C:/Users/jonas/Desktop/T-DAT/03_output/G.T.M.M.png", width = 5, height = 8, dpi = 100)
sprintf("[G_T_M_M] Done!")











## [G_T_M_S] ############################### 
print("[G_T_M_S]...")

# Manipulation des donnees
G.T.M.S <- sourceData %>%
           select(TicketId, MoisVenteId)                                               # Select only ticketId and month
G.T.M.S <- aggregate(G.T.M.S$TicketId, by=list(G.T.M$MoisVenteId), FUN=length) %>%     # Get count of tickets for each months
           rename(MoisVenteId=Group.1, n=x)                                            # Renaming columns
G.T.M.S <- sd(G.T.M.S$n) %>%                                                           # Get standard deviation
           data.frame()                                                                # Make it a dataframe
colnames(G.T.M.S) <- c("sd")                                                           # Give column name for CSV

# Sauvegarde des donnees
write_csv(G.T.M.S, "C:/Users/jonas/Desktop/T-DAT/03_output/G.T.M.S.csv")

# Creation du graphique
df <- data.frame(x = 1, y = G.T.M.S[1,1])
plot <- ggplot(df, aes(x=x,y=y))+
  geom_segment( aes(x=x, xend=x, y=0, yend=y), size=1.3, alpha=0.9) +
  labs(
    title = "Deviation standard du nombre de tickets par mois",
    subtitle = "Donnees du dataset KaDo",
    x = "Ann�e",
    y = "Deviation standard du nombre de tickets par mois"
  )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Sauvegarde du graphique
ggsave("C:/Users/jonas/Desktop/T-DAT/03_output/G.T.M.S.png", width = 5, height = 8, dpi = 100)
sprintf("[G_T_M_S] Done!")











## [G_T_M] ############################### 
print("[G_T_M]...")

# Manipulation des donnees
G.T.M <- sourceData %>%
  select(TicketId, MoisVenteId)

G.T.M <- aggregate(G.T.M$TicketId, by=list(G.T.M$MoisVenteId), FUN=length) %>%
         rename(MoisVenteId=Group.1, n=x)

typeof(G.T.M)

hist(G.T.M)

G.T.M[1] <- lapply(G.T.M[1], as.numeric)
typeof(G.T.M$MoisVenteId)

# Creation du graphique
plot <- ggplot(data = G.T.M, aes(x = as.numeric(MoisVenteId), y = n))+
  geom_histogram(stat="identity", fill="steelblue3", color="steelblue3")+
  labs(
    title = "Nombre total de tickets par mois",
    subtitle = "Donnees du dataset KaDo",
    x = "Mois",
    y = "Nombre de tickets"
  )+
  stat_summary()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(
    breaks = moisIds,
    label = moisNoms
  )

# Sauvegarde du graphique
ggsave("C:/Users/jonas/Desktop/T-DAT/03_output/G.T.M.png", width = 12, height = 8, dpi = 100)
print("[G_T_M] Done!")












################################
##   TOTAL DE VENTES PAR MOIS 
################################
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





################################################
##   VENTES DE PRODUITS PAR MOIS 
################################################