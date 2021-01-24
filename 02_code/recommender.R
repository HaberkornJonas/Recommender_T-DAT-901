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
fileConn<-file(paste(projectPath, "03_output/Client_", clientId, "/description.txt", sep=""))


## CLIENT STATISTICAL DESCRIPTION ###############################
print("## Client statistical dedscripton")
writeLines(c("----- STATISTICAL DESCRIPTION -----"), fileConn)
writeLines(c(paste("ClientId:",clientId)), fileConn)

# Add lots of statistics (# tickets, 3 most frequented months, money spend, # products bought, etc.)
# TODO



## CLIENT SEGMENTATION ANALYSIS ###############################
print("## Client segmentation analysis")
writeLines(c("----- SEGMENTATION ANALYSIS -----"), fileConn)

# [S_R] Regularity
# TODO

# [S_S_T] Spendings per tickets
# TODO

# [S_S_I] Spendings per items
# TODO

# [S_F] Prefered product family
# TODO



## CLIENT RECOMMENDATIONS ###############################
print("## Client recommendations")
writeLines(c("----- CLIENT RECOMMENDATIONS -----"), fileConn)

# [R_1] "Other profiles like yours also like..." 
# TODO

# [R_2] "Because you are interested in..." 
# TODO

# [R_3] "Based on your budget..." 
# TODO



## CLEAN UP ###############################
close(fileConn)