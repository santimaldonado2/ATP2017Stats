google_api_key <- "mysecretapikey"
url <- "https://maps.googleapis.com/maps/api/geocode/json?address=--cityname--&sensor=false&key=--apikey--"

library(httr)
library("jsonlite")

json_file <- 'https://datahub.io/sports-data/atp-world-tour-tennis-data/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

tournaments <- tournaments[tournaments$tourney_year == 2017,]
tournaments$tourney_surface[tournaments$tourney_surface == "Hard"] <- paste(tournaments$tourney_surface[tournaments$tourney_surface == "Hard"], tournaments$tourney_condition[tournaments$tourney_surface == "Hard"], sep = "-")

#Tournaments data
json_data$resources[2,"path"]
tournaments <- read.csv(url(json_data$resources[2,"path"]), stringsAsFactors = F)

#Se puede ver que hay torneos de todos los años, por lo que se filtran solo los del año 2017
tournaments <- tournaments[tournaments$tourney_year == 2017,]

#Creo un dataframe con una fila para darle estructura
tournaments_coordinates <- data.frame(tourney_id = "test", lat = -1, long = -1)
for (index in 1:dim(tournaments)[1]){
  #Armo la url con los datos del torneo
  tournament_data <- tournaments[index,c("tourney_id", "tourney_slug", "tourney_location")]
  city_name <- gsub(" ","%20",tournament_data$tourney_location)
  final_url <- gsub("--cityname--", city_name, gsub("--apikey--", google_api_key, url))
  
  #Llamo a la api
  response <- GET(final_url)  
  results <- content(response,as="parsed") 
  #Extraigo resultados necesarios
  lat <- results$results[[1]]$geometry$location[[1]]
  long <- results$results[[1]]$geometry$location[[2]]
  
  #Creo una nueva fila para el dataframe final
  new_row <- data.frame(tourney_id = as.character(tournament_data$tourney_id), lat = lat, long= long)
  
  #añado la fila al dataframe final
  tournaments_coordinates <- rbind(tournaments_coordinates, new_row)
}

#Borro la primer fila para darle estructura
tournaments_coordinates <- tournaments_coordinates[-1,]

#Escribo el archivo final de salida
write.csv(tournaments_coordinates, file = "data/tournaments_coordinates.csv",row.names=FALSE)

