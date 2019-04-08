library("jsonlite")
library(dplyr)

json_file <- 'https://datahub.io/sports-data/atp-world-tour-tennis-data/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# PLAYERS DATA ####
json_data$resources[10,"path"]
player_overview <- read.csv(url(json_data$resources[10,"path"]), stringsAsFactors = FALSE)

colnames(player_overview)
glimpse(player_overview)

# MATCH SCORES DATA ####
json_data$resources[6,"path"]
match_scores <- read.csv(url(json_data$resources[6,"path"]), stringsAsFactors = FALSE)

colnames(match_scores)
summary(match_scores)

length(unique(match_scores$match_id))
## Filtering duplicated matches ####
### Hay 3827 ids, y 3830 partidos, lo que significa que hay ids duplicados. ¿Como es esto posble?
### Si observamos el id de un partido esta compuesto por el año, id del torneo e identificadores de los jugadores.
### Lo que significa que si hay ids repetidos, es que 2 jugadores se han enfrentado dos veces en el mismo torneo
repeated_matches <- match_scores %>% 
  group_by(match_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

### Observamos cuales son los partidos duplicados
match_scores[match_scores$match_id %in% repeated_matches$match_id,]

# Se puede ver que en todos los casos es cierto que se enfrentan 2 veces, pero en el primer caso
# es en qulifies y luego puede ser que el jugador perdedor haya pasado de ronda dada una lesion o baja 
# de otro jugador, por lo que vuelve al cuadro principal, lo que genera "partidos duplicados"
# Como no vamos a analizar partidos en si, sino sumarizaciones, podemos eliminar el partido de menor importancia

match_scores <- match_scores[-c(1434,3740,3829),]
dim(match_scores)

#Ahora nos han quedado 3827 partidos
#Otra detalle a corregir es que todos los id deberian empezar con 2017 ya que es el año en que se jugaron
#Pero vistos los ids, no es siempre eso correcto, por lo que hay que corregir dichos ids.

match_scores[!grepl("^2017", match_scores$match_id),"match_id"] <- gsub("results", "2017", match_scores[!grepl("^2017", match_scores$match_id),"match_id"])


# MATCH STATS DATA ####

json_data$resources[8,"path"]
match_stats <- read.csv(url(json_data$resources[8,"path"]), stringsAsFactors = FALSE)

summary(match_stats)
glimpse(match_stats)

length(unique(match_stats$match_id))

### Se puede observar que pasa lo mismo que en el dataset anterior, por lo que procedemos a eliminar los mismos partidos

repeated_matches <- match_stats %>% 
  group_by(match_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

match_stats[match_stats$match_id %in% repeated_matches$match_id,]
match_stats <- match_stats[-c(1423,3722,3807),]
dim(match_scores)


# TOURNAMENTS DATA ####
json_data$resources[2,"path"]
tournaments <- read.csv(url(json_data$resources[2,"path"]), stringsAsFactors = F)

glimpse(tournaments)
## Filtering 2017 tourneys ####
#Se puede ver que hay torneos de todos los años, por lo que se filtran solo los del año 2017
tournaments <- tournaments[tournaments$tourney_year == 2017,]
tournaments$tourney_surface[tournaments$tourney_surface == "Hard"] <- paste(tournaments$tourney_surface[tournaments$tourney_surface == "Hard"], tournaments$tourney_condition[tournaments$tourney_surface == "Hard"], sep = "-")

## Adding tourney coordinates ####

# Se lee el csv que se genero con el archivo tournaments_coordinates.R,
# que con la api de google busca las coordenadas de las ciudades donde se disputo
# el torneo
tournaments_coordinates <- read.csv("data/tournaments_coordinates.csv")

tournaments <- merge(tournaments, tournaments_coordinates)

####
# PREPARE MATCHES DATA #####

## Merge match_stats and scores ####
matches <- merge(match_stats, match_scores, by = "match_id")

## Adding new columns needed later ####
### total tiebreask = winner tb + loser tb
matches$match_total_tiebreaks <- matches$winner_tiebreaks_won + matches$loser_tiebreaks_won

### player serve games won = player serve games - opponent break points converted
matches$winner_serve_games_won <- matches$winner_service_games_played - matches$loser_break_points_converted
matches$loser_serve_games_won <- matches$loser_service_games_played - matches$winner_break_points_converted

### Añado una columna que indica si se jugo el set decisivo,
### Si en los grand slams se jugaron los 5 sets, entonces el ganador gano el decisivo
### y el perdedor lo jugo y perdio
matches_grand_slam <- matches[matches$tourney_slug %in% c("australian-open", "roland-garros","us-open", "wimbledon"),]
matches_grand_slam$match_deciding_sets <- ifelse(matches_grand_slam$winner_sets_won + matches_grand_slam$loser_sets_won == 5, 1, 0)
matches_grand_slam$winner_deciding_set <- matches_grand_slam$match_deciding_sets

### Lo mismo para el resto de los torneos que son al mejor de 3 sets
matches_not_grand_slam <- matches[!matches$tourney_slug %in% c("australian-open", "roland-garros","us-open", "wimbledon"),]
matches_not_grand_slam$match_deciding_sets <- ifelse(matches_not_grand_slam$winner_sets_won + matches_not_grand_slam$loser_sets_won == 3, 1, 0)
matches_not_grand_slam$winner_deciding_set <- matches_not_grand_slam$match_deciding_sets

matches_grand_slam <- matches_grand_slam[c("match_id", "match_deciding_sets", "winner_deciding_set")]
matches_not_grand_slam <- matches_not_grand_slam[c("match_id", "match_deciding_sets", "winner_deciding_set")]

matches_deciding_sets <- rbind(matches_grand_slam, matches_not_grand_slam)
matches <- merge(matches, matches_deciding_sets, by = "match_id")

matches$match_deciding_sets
matches$loser_deciding_set <- 0

## Create a "long" dataset ####
## En vez de tener una fila por partido, con los datos del ganador y del perdedor,
## Se hace una fila por cada partido y jugador, con una columna nueva si gano o perdio
matches_colnames <- colnames(matches)
tourney_cols <- matches_colnames[grepl("^tourney", matches_colnames)]

match_cols <- matches_colnames[grepl("^match", matches_colnames)]

round_cols <- matches_colnames[grepl("^round", matches_colnames)]

winner_cols <- matches_colnames[grepl("^winner", matches_colnames)]

loser_cols <- matches_colnames[grepl("^loser", matches_colnames)]


matches_winners <- matches[c(tourney_cols, round_cols, match_cols, winner_cols)]
colnames(matches_winners) <- gsub("winner_", "", colnames(matches_winners))
matches_winners$match_result <- "W"

matches_losers <- matches[c(tourney_cols, round_cols, match_cols, loser_cols)]
colnames(matches_losers) <- gsub("loser_", "", colnames(matches_losers))
matches_losers$match_result <- "L"

matches_long <- rbind(matches_winners, matches_losers)
matches_long <- matches_long[!colnames(matches_long) %in% c("tourney_order.y", "tourney_url_suffix", "match_stats_url_suffix.x", "match_stats_url_suffix.y")]
colnames(matches_long) <- gsub(".x", "", colnames(matches_long))

## Adding tournament info

tournaments_to_merge <- tournaments[c("tourney_id", "tourney_slug", "tourney_name", "tourney_surface", "tourney_lat", "tourney_long")]
matches_long <- merge(matches_long, tournaments_to_merge)

matches_long$tourney_surface <- as.factor(matches_long$tourney_surface)
matches_long$match_result <- factor(matches_long$match_result, levels = c("W","L"), labels = c("Won", "Lost"))

## Se ordenan cronologicamente
## Primero por orden de los torneos, luego por orden inverso de la ronda 
## y luego por orden de los partidos en la misma ronda
matches_long <- matches_long %>% 
  arrange(tourney_order, desc(round_order), match_order)

matches_long$match_played <- 1

## Filtering players_overview ####
players_overview_2017 <- player_overview[player_overview$player_id %in% unique(matches_long$player_id),]


# Write output files ####
write.csv(matches_long, file = "data/matches_long.csv",row.names=FALSE)
write.csv(players_overview_2017, file = "data/players_overview.csv", row.names = FALSE)
write.csv(tournaments, file = "data/tournaments.csv", row.names = FALSE)


