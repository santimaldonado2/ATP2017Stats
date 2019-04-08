rm(list=ls()) 
library("jsonlite")
library(dplyr)
library(ggplot2)
library(RColorBrewer)

matches_long <- read.csv("data/matches_long.csv", stringsAsFactors = F, encoding="UTF-8")
players_overview_2017 <- read.csv("data/players_overview.csv", stringsAsFactors = F, encoding="UTF-8")
tournaments <- read.csv("data/tournaments.csv", stringsAsFactors = F, encoding="UTF-8")

matches_long$tourney_surface <- as.factor(matches_long$tourney_surface)
matches_long$match_result <- factor(matches_long$match_result, levels = c("Won","Lost"), labels = c("Won", "Lost"))

colnames(matches_long)
# =========== STATIC VISUALIZATION =============
#### PLAYER MATCHES #####
player_matches <- matches_long %>% 
  filter(slug == "rafael-nadal") %>% 
  mutate(match_number = 1) %>% 
  group_by(tourney_slug, tourney_order) %>% 
  mutate(time_played = cumsum(match_duration),
         matches_played = cumsum(match_played),
         cum_first_serves_in = cumsum(first_serves_in),
         cum_first_serves_total = cumsum(first_serves_total),
         cum_first_serve_points_won = cumsum(first_serve_points_won),
         cum_first_serve_points_total = cumsum(first_serve_points_total),
         cum_second_serve_points_won = cumsum(second_serve_points_won),
         cum_second_serve_points_total = cumsum(second_serve_points_total),
         cum_serve_games_won = cumsum(serve_games_won),
         cum_service_games_played = cumsum(service_games_played),
         cum_aces = cumsum(aces),
         cum_double_faults = cumsum(double_faults)) %>% 
  mutate(cum_first_serve_in_ratio = 100*cum_first_serves_in/ cum_first_serves_total,
         cum_first_serve_points_won_ratio = 100*cum_first_serve_points_won/ cum_first_serve_points_total,
         cum_second_serve_points_won_ratio = 100*cum_second_serve_points_won/ cum_second_serve_points_total, 
         cum_serve_games_won_ratio = 100*cum_serve_games_won  / cum_service_games_played,
         cum_average_aces_match = cum_aces / matches_played, 
         cum_average_double_faults = cum_double_faults / matches_played) %>%
  mutate(cum_first_serve_return_won = cumsum(first_serve_return_won),
         cum_first_serve_return_total = cumsum(first_serve_return_total),
         cum_second_serve_return_won = cumsum(second_serve_return_won),
         cum_second_serve_return_total = cumsum(second_serve_return_total),
         cum_break_points_converted = cumsum(break_points_converted),
         cum_return_games_played = cumsum(return_games_played),
         cum_break_points_return_total = cumsum(break_points_return_total)) %>% 
  mutate(cum_first_serve_return_ratio = 100*cum_first_serve_return_won / cum_first_serve_return_total,
         cum_second_serve_return_ratio = 100*cum_second_serve_return_won / cum_second_serve_return_total,
         cum_return_games_won_ratio = 100*cum_break_points_converted / cum_return_games_played,
         cum_break_points_converted_ratio = 100*cum_break_points_converted / cum_break_points_return_total) %>% 
  #Under Pressure Variables
  mutate(cum_break_points_saved = cumsum(break_points_saved),
         cum_break_points_serve_total = cumsum(break_points_serve_total),
         cum_tiebreaks_won = cumsum(tiebreaks_won),
         cum_match_total_tiebreaks = cumsum(match_total_tiebreaks),
         cum_deciding_set = cumsum(deciding_set),
         cum_match_deciding_sets = cumsum(match_deciding_sets)) %>%
  mutate(cum_break_points_saved_ratio = 100*cum_break_points_saved / cum_break_points_serve_total,
         cum_tiebreaks_won_ratio = 100*cum_tiebreaks_won / cum_match_total_tiebreaks,
         cum_deciding_sets_won_ratio = 100*cum_deciding_set / cum_match_deciding_sets)
player_matches[is.na(player_matches)] <-0

player_matches <- player_matches %>% 
  mutate(serve_performance = cum_first_serve_in_ratio + cum_first_serve_points_won_ratio + cum_second_serve_points_won_ratio + cum_serve_games_won_ratio + cum_average_aces_match - cum_average_double_faults,
         return_performance = cum_first_serve_return_ratio +  cum_second_serve_return_ratio +  cum_return_games_won_ratio + cum_break_points_converted_ratio,
         under_pressure_performance = cum_break_points_converted_ratio +  cum_break_points_saved_ratio +  cum_tiebreaks_won_ratio + cum_deciding_sets_won_ratio)
  

player_matches$match_number <- c(1:nrow(player_matches))


select_cols <- unlist(lapply(matches_long, is.numeric))
select_cols["slug"] <- TRUE
select_cols["player_id"] <- TRUE

sum_match_stats_long <- matches_long[select_cols] %>% 
  group_by(player_id, slug) %>% 
  summarise_all(funs(sum)) %>% 
  #Serve Variables
  mutate(first_serve_in_ratio = 100*first_serves_in/ first_serves_total,
         first_serve_points_won_ratio = 100*first_serve_points_won/ first_serve_points_total,
         second_serve_points_won_ratio = 100*second_serve_points_won/ second_serve_points_total, 
         serve_games_won_ratio = 100*serve_games_won  / service_games_played,
         average_aces_match = aces / match_played, 
         average_double_faults = double_faults / match_played) %>%
  #Return Variables
  mutate(first_serve_return_ratio = 100*first_serve_return_won / first_serve_return_total,
         second_serve_return_ratio = 100*second_serve_return_won / second_serve_return_total,
         return_games_won_ratio = 100*break_points_converted / return_games_played,
         break_points_converted_ratio = 100*break_points_converted / break_points_return_total) %>% 
  #Under Pressure Variables
  mutate(break_points_saved_ratio = 100*break_points_saved / break_points_serve_total,
         tiebreaks_won_ratio = 100*tiebreaks_won / match_total_tiebreaks,
         deciding_sets_won_ratio = 100*deciding_set / match_deciding_sets)

sum_match_stats_long[is.na(sum_match_stats_long)] <- 0
sum_match_stats_long <- sum_match_stats_long %>% 
  mutate(serve_performance = first_serve_in_ratio + first_serve_points_won_ratio + second_serve_points_won_ratio + serve_games_won_ratio + average_aces_match - average_double_faults ) %>% 
  mutate(return_performance = first_serve_return_ratio +  second_serve_return_ratio +  return_games_won_ratio + break_points_converted_ratio) %>% 
  mutate(under_pressure_performance = break_points_converted_ratio +  break_points_saved_ratio +  tiebreaks_won_ratio + deciding_sets_won_ratio)


select_cols["tourney_surface"] <- TRUE
sum_match_stats_long_by_tournament <- matches_long[select_cols] %>% 
  group_by(player_id, slug, tourney_surface) %>% 
  summarise_all(funs(sum)) %>% 
  #Serve Variables
  mutate(first_serve_in_ratio = 100*first_serves_in/ first_serves_total,
         first_serve_points_won_ratio = 100*first_serve_points_won/ first_serve_points_total,
         second_serve_points_won_ratio = 100*second_serve_points_won/ second_serve_points_total, 
         serve_games_won_ratio = 100*serve_games_won  / service_games_played,
         average_aces_match = aces / match_played, 
         average_double_faults = double_faults / match_played) %>%
  #Return Variables
  mutate(first_serve_return_ratio = 100*first_serve_return_won / first_serve_return_total,
         second_serve_return_ratio = 100*second_serve_return_won / second_serve_return_total,
         return_games_won_ratio = 100*break_points_converted / return_games_played,
         break_points_converted_ratio = 100*break_points_converted / break_points_return_total) %>% 
  #Under Pressure Variables
  mutate(break_points_saved_ratio = 100*break_points_saved / break_points_serve_total,
         tiebreaks_won_ratio = 100*tiebreaks_won / match_total_tiebreaks,
         deciding_sets_won_ratio = 100*deciding_set / match_deciding_sets)

sum_match_stats_long_by_tournament[is.na(sum_match_stats_long_by_tournament)] <- 0
sum_match_stats_long_by_tournament <- sum_match_stats_long_by_tournament %>% 
  mutate(serve_performance = first_serve_in_ratio + first_serve_points_won_ratio + second_serve_points_won_ratio + serve_games_won_ratio + average_aces_match - average_double_faults ) %>% 
  mutate(return_performance = first_serve_return_ratio +  second_serve_return_ratio +  return_games_won_ratio + break_points_converted_ratio) %>% 
  mutate(under_pressure_performance = break_points_converted_ratio +  break_points_saved_ratio +  tiebreaks_won_ratio + deciding_sets_won_ratio)

player_stats <- sum_match_stats_long %>% 
  filter(slug == "rafael-nadal")

sum_select_cols <- unlist(lapply(matches_long, is.numeric))

players_means <- matches_long[sum_select_cols] %>% 
  summarise_all(funs(sum)) %>% 
  #Serve Variables
  mutate(first_serve_in_ratio = 100*first_serves_in/ first_serves_total,
         first_serve_points_won_ratio = 100*first_serve_points_won/ first_serve_points_total,
         second_serve_points_won_ratio = 100*second_serve_points_won/ second_serve_points_total, 
         serve_games_won_ratio = 100*serve_games_won  / service_games_played,
         average_aces_match = aces / match_played, 
         average_double_faults = double_faults / match_played) %>%
  #Return Variables
  mutate(first_serve_return_ratio = 100*first_serve_return_won / first_serve_return_total,
         second_serve_return_ratio = 100*second_serve_return_won / second_serve_return_total,
         return_games_won_ratio = 100*break_points_converted / return_games_played,
         break_points_converted_ratio = 100*break_points_converted / break_points_return_total) %>% 
  #Under Pressure Variables
  mutate(break_points_saved_ratio = 100*break_points_saved / break_points_serve_total,
         tiebreaks_won_ratio = 100*tiebreaks_won / match_total_tiebreaks,
         deciding_sets_won_ratio = 100*deciding_set / match_deciding_sets)

players_means <- players_means %>% 
  mutate(serve_performance = first_serve_in_ratio + first_serve_points_won_ratio + second_serve_points_won_ratio + serve_games_won_ratio + average_aces_match - average_double_faults ) %>% 
  mutate(return_performance = first_serve_return_ratio +  second_serve_return_ratio +  return_games_won_ratio + break_points_converted_ratio) %>% 
  mutate(under_pressure_performance = break_points_converted_ratio +  break_points_saved_ratio +  tiebreaks_won_ratio + deciding_sets_won_ratio)

## Customize W-L circle color
results_color <- c("#336600", "#CC0000")
names(results_color) <- c(levels(player_matches$match_result))
colScale <- scale_colour_manual(name = "Match Result", values = results_color)


#### TOURNAMENTS RECTANGLES ####

player_tournaments <- player_matches %>% 
  group_by(tourney_order, tourney_surface, tourney_slug) %>% 
  summarise(xmin = min(match_number) - 0.5,
            xmax = max(match_number) + 0.5,
            best_round = min(round_order))

player_won_finals <- player_matches %>% 
  filter(round_order == 1) %>%
  filter(match_result == "Won")

player_tournaments$ymin <- min(player_matches$serve_performance) - 110
player_tournaments$ymax <- max(player_matches$serve_performance) + 30

player_tournaments$ymin_return <- min(player_matches$return_performance) - 110
player_tournaments$ymax_return <- max(player_matches$return_performance) + 50

player_tournaments$ymin_under_pressure <- min(player_matches$under_pressure_performance) - 110
player_tournaments$ymax_under_pressure <- max(player_matches$under_pressure_performance) + 30

simpleCap <- function(x) {
  if(grepl("-", x)){
    s <- strsplit(x, "-")[[1]]
    name <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse="\n")
  } else{
    name <-  paste0(toupper(substring(x, 1,1)), substring(x, 2))
  }
  name
}

player_tournaments$tourney_slug <- sapply(as.character(player_tournaments$tourney_slug), simpleCap)

## Asign tournament label positon
margin <- c(5, 20)
player_tournaments$xlabel <- (player_tournaments$xmin + player_tournaments$xmax)/ 2
player_tournaments$ylabel <- player_tournaments$ymax - margin

player_tournaments$xlabel_return <- (player_tournaments$xmin + player_tournaments$xmax)/ 2
player_tournaments$ylabel_return <- player_tournaments$ymax_return - margin

player_tournaments$xlabel_under_pressure <- (player_tournaments$xmin + player_tournaments$xmax)/ 2
player_tournaments$ylabel_under_pressure <- player_tournaments$ymax_under_pressure - margin


## Customize Surfaces color
surfaces_colors <- c("#FF6600", "#33CC33","#0033CC", "#3399FF")
names(surfaces_colors) <- c(levels(player_matches$tourney_surface))
surfScale <- scale_fill_manual(name = "Court Surface", values = surfaces_colors)


#### TIME PLAYED BARS ####
bars_top = min(player_matches$serve_performance) - 10
bars_bottom = min(player_matches$serve_performance) - 110
max_size = bars_top - bars_bottom

bars_top_return = min(player_matches$return_performance) - 10
bars_bottom_return = min(player_matches$return_performance) - 110
max_size_return = bars_top_return - bars_bottom_return

bars_top_under_pressure = min(player_matches$under_pressure_performance) - 10
bars_bottom_under_pressure = min(player_matches$under_pressure_performance) - 110
max_size_under_pressure = bars_top_under_pressure - bars_bottom_under_pressure

player_mins_played <- data.frame()
player_mins_played <- player_matches %>% 
  select(time_played, tourney_order, match_number, tourney_slug) %>% 
  mutate(ymin = bars_bottom,
         ymin_return = bars_bottom_return,
         ymin_under_pressure = bars_bottom_under_pressure)

player_mins_played$ymax <- (max_size * player_matches$time_played / max(player_matches$time_played) ) + bars_bottom
player_mins_played$ymax_return <- (max_size * player_matches$time_played / max(player_matches$time_played) ) + bars_bottom_return
player_mins_played$ymax_under_pressure <- (max_size * player_matches$time_played / max(player_matches$time_played) ) + bars_bottom_under_pressure


#### LABELS ####
labels <- data.frame(x = -1, 
                     y = player_mins_played[1,]$ymax + 70, 
                     y_return = player_mins_played[1,]$ymax + 70, 
                     label = "Cumulative\nMinutes\nPlayed\nBy\nTournament")
labels <- rbind(labels, data.frame(x = player_won_finals$match_number[1] - 2, 
                         y = player_matches$serve_performance[player_matches$match_number == player_won_finals$match_number[1]] + 30,
                         y_return = player_matches$return_performance[player_matches$match_number == player_won_finals$match_number[1]] + 30,
                         label = "Champion"))

#### ARROWS #####
champion_arrow_origin <- player_matches[player_matches$match_number == player_won_finals$match_number[1], c("match_number", "serve_performance")]
names(champion_arrow_origin) <- c("x","y")

champion_arrow_end <- labels[2, c("x","y")]
names(champion_arrow_end) <- c("xend", "yend")
champion_arrow_end$yend <- champion_arrow_end$yend - 5
champion_arrow_end$curvature <- -0.3

arrows <- cbind(champion_arrow_origin, champion_arrow_end)

minutes_arrow <- labels[1, c("x","y")]
minutes_arrow$x <- minutes_arrow$x + 1

minutes_arrow_end <- minutes_arrow
names(minutes_arrow_end) <- c("xend", "yend")
minutes_arrow_end$xend <- minutes_arrow_end$xend  + 6
minutes_arrow_end$yend <- minutes_arrow_end$yend  - 20
minutes_arrow_end$curvature <- 0.6
minutes_arrow <- cbind(minutes_arrow, minutes_arrow_end)

arrows <-rbind(arrows, minutes_arrow)
#### 

#### Theme Edition ####
# Obtención de tema por defecto
default.theme <- theme_get()
default.theme$panel.background


atp.theme <- default.theme + 
  theme(panel.background = element_rect(fill = "#ecf0f5"),
        plot.background = element_rect(fill = "#ecf0f5"),
        axis.line.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(hjust=0.65 ),
        axis.ticks.y = element_blank(),
        legend.title = element_text(face="bold"),
        legend.key = element_blank(),
        legend.background = element_rect(fill = "#ecf0f5"),
        axis.title = element_text(face="bold"),
        plot.title = element_text(hjust = 0.5))

#### PLOTTING #######
  # Serve plot ----
serve_plot <- ggplot(data = player_matches) +
  geom_rect(data = player_tournaments, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill=tourney_surface), colour="#888888", alpha = 0.5) +
  geom_curve(data = arrows, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.03, "npc")), curvature = -0.3, size = 1) +
  geom_line(aes(x= match_number, y = serve_performance)) +
  geom_hline(data = player_stats, aes(yintercept=serve_performance), linetype="dashed", size = 1) +
  geom_text(data = player_stats, aes(y = serve_performance + 1), x = -3, label = "Rafa Avg\nServ.Perf.", size = 3) +
  geom_hline(data = players_means, aes(yintercept=serve_performance), linetype="dotted", size = 1) +
  geom_text(data = players_means, aes(y = serve_performance + 1), x = -1, label = "All Players Avg\nServ.Perf.", size = 3) +
  geom_point(data = player_won_finals, aes(x=match_number, y=serve_performance), colour = "#E6E600", size = 4) +
  geom_point(aes(x= match_number, y = serve_performance, colour = match_result), size = 3) + 
  geom_text(aes(x= match_number, y = serve_performance, label = round_order), size = 2.5, colour = "#FFFFFF", fontface = "bold") + 
  geom_segment(data = player_mins_played, aes(x = match_number, y = ymin, xend = match_number, yend = ymax), size = 2) + 
  geom_text(data = player_mins_played, aes(x = match_number, y = ymax + 5, label = as.character(time_played)), size = 3) +
  geom_text(data = player_tournaments, aes(x = xlabel , y = ylabel, label = tourney_slug), size = 3, fontface = "bold") +
  geom_text(data = labels, aes(x=x, y = y, label = label), fontface = "bold", size = 2.5) +
  ggtitle(paste0(player_matches$name[1],"'s Service Performance - 2017")) +
  ylab("Service Performance") + 
  xlab("Matches Played During 2017") +
  scale_y_continuous(expand = c(0,0), breaks = seq(round(min(player_matches$serve_performance) / 10) * 10, ceiling(max(player_matches$serve_performance)/ 10) * 10, 35)) +
  surfScale +
  colScale +
  atp.theme
serve_plot
  # return plot ----
return_plot <- ggplot(data = player_matches) +
  geom_rect(data = player_tournaments, aes(xmin = xmin, ymin = ymin_return, xmax = xmax, ymax = ymax_return, fill=tourney_surface), colour="#888888", alpha = 0.5) +
  geom_line(aes(x= match_number, y = return_performance)) +
  geom_hline(data = player_stats, aes(yintercept=return_performance), linetype="dashed", size = 1) +
  geom_text(data = player_stats, aes(y = return_performance + 1), x = -1, label = "Rafa Avg\nReturn.Perf.", size = 3) +
  geom_hline(data = players_means, aes(yintercept=return_performance), linetype="dotted", size = 1) +
  geom_text(data = players_means, aes(y = return_performance + 1), x = 0, label = "All Players Avg\nReturn.Perf.", size = 3) +
  geom_point(data = player_won_finals, aes(x=match_number, y=return_performance), colour = "#E6E600", size = 4) +
  geom_point(aes(x= match_number, y = return_performance, colour = match_result), size = 3) + 
  geom_text(aes(x= match_number, y = return_performance, label = round_order), size = 2.5, colour = "#FFFFFF", fontface = "bold") + 
  geom_segment(data = player_mins_played, aes(x = match_number, y = ymin_return, xend = match_number, yend = ymax_return), size = 2) + 
  geom_text(data = player_mins_played, aes(x = match_number, y = ymax_return + 5, label = as.character(time_played)), size = 3) +
  geom_text(data = player_tournaments, aes(x = xlabel_return , y = ylabel_return, label = tourney_slug), size = 3, fontface = "bold") +
  #geom_text(data = labels, aes(x=x, y = y, label = label), fontface = "bold", size = 2.5) +
  ggtitle(paste0(player_matches$name[1],"'s Return Performance - 2017")) +
  ylab("Return Performance") + 
  xlab("Matches Played During 2017") +
  scale_y_continuous(expand = c(0,0), breaks = seq(round(min(player_matches$return_performance) / 10) * 10, ceiling(max(player_matches$return_performance)/ 10) * 10, 35)) +
  surfScale +
  colScale +
  atp.theme

return_plot
 #Under Pressure plot ----
under_pressure_plot <- ggplot(data = player_matches) +
  geom_rect(data = player_tournaments, aes(xmin = xmin, ymin = ymin_under_pressure, xmax = xmax, ymax = ymax_under_pressure, fill=tourney_surface), colour="#888888", alpha = 0.5) +
  geom_line(aes(x= match_number, y = under_pressure_performance)) +
  geom_hline(data = player_stats, aes(yintercept=under_pressure_performance), linetype="dashed", size = 1) +
  geom_text(data = player_stats, aes(y = under_pressure_performance + 1), x = -1, label = "Rafa Avg\n U.P. Perf.", size = 3) +
  geom_hline(data = players_means, aes(yintercept=under_pressure_performance), linetype="dotted", size = 1) +
  geom_text(data = players_means, aes(y = under_pressure_performance + 1), x = 0, label = "All Players Avg\n U.P. Perf.", size = 3) +
  geom_point(data = player_won_finals, aes(x=match_number, y=under_pressure_performance), colour = "#E6E600", size = 4) +
  geom_point(aes(x= match_number, y = under_pressure_performance, colour = match_result), size = 3) + 
  geom_text(aes(x= match_number, y = under_pressure_performance, label = round_order), size = 2.5, colour = "#FFFFFF", fontface = "bold") + 
  geom_segment(data = player_mins_played, aes(x = match_number, y = ymin_under_pressure, xend = match_number, yend = ymax_under_pressure), size = 2) + 
  geom_text(data = player_mins_played, aes(x = match_number, y = ymax_under_pressure + 5, label = as.character(time_played)), size = 3) +
  geom_text(data = player_tournaments, aes(x = xlabel_under_pressure , y = ylabel_under_pressure, label = tourney_slug), size = 3, fontface = "bold") +
  #geom_text(data = labels, aes(x=x, y = y, label = label), fontface = "bold", size = 2.5) +
  ggtitle(paste0(player_matches$name[1],"'s Under Pressure Performance - 2017")) +
  ylab("Under Pressure Performance") + 
  xlab("Matches Played During 2017") +
  scale_y_continuous(expand = c(0,0), breaks = seq(round(min(player_matches$under_pressure_performance) / 10) * 10, ceiling(max(player_matches$under_pressure_performance)/ 10) * 10, 35)) +
  surfScale +
  colScale +
  atp.theme

under_pressure_plot



#================================================
# =========== Dinamic Visualization =============



#### Data preparation for dynamic visualization ####

### Match stats long ####

colnames(matches_long)



### Tournaments By Player ####
tournaments_by_player <- matches_long %>% 
  group_by(player_id, slug, tourney_id, tourney_lat, tourney_long, tourney_surface, tourney_name) %>% 
  summarise(round_order = min(round_order))

colnames_to_merge <- colnames(tournaments_by_player)
tournaments_by_player <- merge(tournaments_by_player, matches_long[c(colnames_to_merge,"tourney_round_name", "match_result")], by = colnames_to_merge)


### Players list ####
player_options <- players_overview_2017 %>% 
  filter(player_id %in% sum_match_stats_long$player_id) %>% 
  mutate(complete_name  = paste(first_name, last_name, sep = " ")) %>% 
  select(complete_name, player_id) 

player_select_choices <- player_options$player_id
names(player_select_choices) <- player_options$complete_name

surface_list <- levels(sum_match_stats_long_by_tournament$tourney_surface[1])
names(surface_list) <- surface_list
surface_list["All Surfaces"] <- "all"

players_overview_2017[is.na(players_overview_2017)] <- "-"
players_overview_2017$handedness[players_overview_2017$handedness == ""] <- "-"
players_overview_2017$backhand[players_overview_2017$backhand == ""] <- "-"
####################################################