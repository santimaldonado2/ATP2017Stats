library(shiny)
library(shinydashboard)
library(leaflet)
ui <-  dashboardPage(
  skin = 'yellow',
  # Dashboard Header ----
  dashboardHeader(title = 'ATP 2017 Stats',
                  titleWidth = 250),
  # Dashborad Side Bar ----
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem('Psycology or Talent', tabName = 'player_graph'),
                     menuItem('Players Stats', tabName = 'player_stats_tab')
                   )),
  # Dashboard Body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    # Tab Items ----
    tabItems(
      # Player Stats Tab ----
      tabItem(tabName = 'player_stats_tab',
              # Fluid Row ----
              fluidRow(
                #Column ----
                column(width = 4, 
                       #Player Selector ----
                       selectInput("player_selector", 
                                   label = h3("Choose a Player"), 
                                   choices = player_select_choices
                       )
                ),
                column(width = 4, 
                       #Surface Selector ----
                       selectInput("surface_selector", 
                                   label = h3("Choose a Surface"), 
                                   choices = surface_list
                       )
                )
              ),
              # Player Info Section ----
              fluidRow(
                h2("Player Info", class = "main-title"),
                wellPanel(
                  class = "player_info_panel",
                  fluidRow(
                    column(
                      class = "subsection",
                      width = 2,
                      htmlOutput("player_img")
                    ),
                    column(
                      class = "subsection player_info_subsection",
                      width = 2,
                      h3("Handedness", class = "section-title"),
                      tags$div(
                        class = "subsection-number",
                        textOutput("player_handedness")
                      )
                    ),
                    column(
                      class = "subsection player_info_subsection",
                      width = 2,
                      h3("Backhand", class = "section-title"),
                      tags$div(
                        class = "subsection-number",
                        textOutput("player_backhand")
                      )
                    ),
                    column(
                      class = "subsection player_info_subsection",
                      width = 2,
                      h3("Height (cm)", class = "section-title"),
                      tags$div(
                        class = "subsection-number",
                        textOutput("player_height")
                      )
                    ),
                    column(
                      class = "subsection player_info_subsection",
                      width = 2,
                      h3("Weight (Kg)", class = "section-title"),
                      tags$div(
                        class = "subsection-number",
                        textOutput("player_weight")
                      )
                    ),
                    column(
                      class = "subsection player_info_subsection",
                      width = 2,
                      h3("Pro Since", class = "section-title"),
                      tags$div(
                        class = "subsection-number",
                        textOutput("player_pro_since")
                      )
                    )
                  )
                )
              ),
              # Panels Section ----
              h2("Player Stats", class = "main-title"),
              fluidRow(
                # Serve Section ----
                column(width = 4,
                       wellPanel(
                         class = "section",
                         #Header Row ----
                         fluidRow(
                           # Section Title ----
                           h3("Serve Performance", class = "section-title"),
                           # Section Number ----
                           tags$div(
                             class = "section-number",
                             textOutput("serve_performance")
                             ),
                           # Subsection Row ----
                           fluidRow(
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 4,
                               h5("% 1st Serve", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("first_serve_in_ratio")
                               )
                             ),
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 4,
                               h5("% 1st Serve Points Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("first_serve_points_won_ratio")
                               )
                             ),
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 4,
                               h5("% 2nd Serve Points Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("second_serve_points_won_ratio")
                               )
                             )
                           ),
                           # Subsection Row ----
                           fluidRow(
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 4,
                               h5("% Service Games Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("serve_games_won_ratio")
                               )
                             ),
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 4,
                               h5("Avg. Aces", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("average_aces_match")
                               )
                             ),
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 4,
                               h5("Avg Double Faults", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("average_double_faults")
                               )
                             )
                           )
                           #End Subsection Row ----
                         )
                         #End Header Row ----
                       )
                       
                ),
                #End Serve Section ----
                #Return Section ----
                column(width = 4,
                       wellPanel(
                         class = "section",
                         fluidRow(
                           # Return Section Title ----
                           h3("Return Performance", class = "section-title"),
                           # Return Section Number ----
                           tags$div(
                             class = "section-number",
                             textOutput("return_performance")
                             ),
                           # SubSection Row ----
                           fluidRow(
                             #Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% 1st Serve Return Points Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("first_serve_return_ratio")
                               )
                             ),
                             #Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% 2nd Serve Return Points Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("second_serve_return_ratio")
                               )
                             )
                           ),
                           # SubSection Row ----
                           fluidRow(
                             #Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% Return Games Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("return_games_won_ratio")
                               )
                             ),
                             #Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% Break Points Converted", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("break_points_converted_ratio")
                               )
                             )
                           )
                           # End Subsection Row ----
                         )
                       )
                ),
                #End Return Section ----
                # Under Pressure Section ----
                column(width = 4,
                       wellPanel(
                         class = "section",
                         fluidRow(
                           #Under Pressure Section Title ----
                           h3("Under Pressure Performance", class = "section-title"),
                           #Under Pressure Section Number ----
                           tags$div(
                             class = "section-number",
                             textOutput("under_pressure_performance")
                             ),
                           # Subsection Row ----
                           fluidRow(
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% Break Points Saved", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("break_points_saved_ratio")
                               )
                             ),
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% Tiebreaks Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("tiebreaks_won_ratio")
                               )
                             )
                           ),
                           #Subsection Row ----
                           fluidRow(
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% Break Points Converted", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("break_points_converted_ratio2")
                               )
                             ),
                             # Subsection ----
                             column(
                               class = "subsection",
                               width = 6,
                               h5("% Deciding Sets Won", class = "subsection-title"),
                               tags$div(
                                 class = "subsection-number",
                                 textOutput("deciding_sets_won_ratio")
                               )
                             )
                           )
                           # End Subsection Row ----
                         )
                       )
                )
                #End Under Pressure Section ----
                
              ),
              # End Panels Section ----
              # Map Section ----
              fluidRow(
                h2("Tournaments Played", class = "main-title"),
                leafletOutput(outputId = 'mapa')
              )
              # End Map Section ----
      ),
      #End Stats Tab ----
      #Graph Tab ----
      tabItem(tabName = 'player_graph',
              h2("Psychology or Talent?", class = "main-title"),
              #Introduction ----
              p('In professional tennis, as in all the elite sports, the players spend hours and hours training, aiming to improve their physical resistance and their tennis skills. But it is also said that "Tennis is 20% talent and 80% mind". Is this true?'),
              p("Here I'm going to analyze match's statistics of the ATP (Association of Tennis Professionals) played during the year 2017, in order to find out what is more important, mind or body. For this purpose I'm going to use three main metrics that the ATP messsures frequently:",
                tags$ul(
                  tags$li(tags$span("Serve Performance: ", class = "item-title"),
                          "Is a combination of those statistics related to the service. These are: first serves in percentage, first serve points won, second serve points won, service games won, average aces per match and penalized by average double faults per match."), 
                  tags$li(tags$span("Return Performance: ", class = "item-title"),
                          "This metric tells how well the professional plays when is under the opponent service. So it's a combination of: first and second serve return points won percentage, return games won and percentage of break points converted."), 
                  tags$li(tags$span("Under Pressure Performance: ", class = "item-title"),
                          "In this case the most important metrics are those which take into consideration the match moments when the participants are under very high pressure. So it's a sum of: Break points converted and saved, tiebreaks won and deciding sets won, it means in a long match if the player won or lost the fifth set, or the third set in short matches.")
                )),
              p("Due to the nature of the statistics aforementioned, they can not be compared as they are in different scales. Regardless of that, all players can be compared in the same metric; and it is also possible to analyze how they change during the year, in different surfaces;  and if they are more affected by fatigue or pressure."),
              p("As it's impossible to analyze all players, I'm going to focus just in one of them: Rafael Nadal. He is one of the best players in tennis history and also considered as a very complete professional, it means, he is capable of doing well in service, return and under pressure."),
              # Serve Performance ----
              h3("Serve Performance", class = "main-title"),
              p("Firstly I'm going to look over Nadal's serve. In the graph below, you can see how his performance had evolved through the matches he played in the different tourneys."),
              plotOutput("serve_graph"),
              p("This graph contains a lot of information, so a big deal of insight can be gained from it. The first of them is: If you want to be a top performer, be better than the rest. It can be observed the average serve performance of Rafa it's much higher than the average of all the players."),
              p("If I focus on the first hard surface tournaments, it is possible to see that his performance tends to go down as the matches go by in each tourney. Another thing to take into consideration is that he reached the final in three tournaments and he lost them performing one of the worst serves in each competition. That's to say that the fatigue plays its part in this metric."),
              p("Then I continue with the clay tourneys. It's well know that Nadal is considered the \"Clay King\" but, surprisingly, he performed approximately half of the matches above his average and the other half below of it, whereas in the first part all but three matches he did it better than his average. So three main insights come to my mind. First, the service performance is not the main component to win a match, Rafa did it worse in clay than in hard, but he won four out of five tourneys he played. Second, the tiredness is much more important in hard surface than in clay. While in the cement competitions the performance decreases in every match, in the clay ones his metric tends to raise as the matches go by. Finally, doing it well in service is strictly related to the kind of surface where they play. That's why the courts with hard surface are called \"fast courts\" and the clay ones \"slow courts\". So in the former the ball bounce much faster, making harder to return the service and then the serve performance is higher, whereas in the latter, is much easier to give back the ball since it loses speed when it bounces."),
              # Serve Performance ----
              h3("Return Performance", class = "main-title"),
              plotOutput("return_graph"),
              p("Analyzing this graph, it could be noticed that Nadal's behaviour change drastically compared to the previous one. Once again it can be seen that Rafa's average is higher than all players average, but in this case his performance on clay is much better than in hard surface. it is clearly related to the aspect mentioned before about how the clay surface gets down the serve performance, so if the opponent is serving worse, it will be easier for Rafa to win those points."),
              p("Another interesting insight is that in every match, but three, when Nadal's performance was above his average, he won. Even more, the only match that he lost on clay was the one with worst return performance on that surface."),
              p("Does tiredness worsen the return? It cannot be concluded. In several tourneys the numbers go down, accussing some fatigue. But in other cases, it raises as the matches go by, maybe because the opponent is tired too, so he performs worse when he serves, giving more opportunities to break his service."),
              p("In conclusion, is not enough to be good at serving, it's a little bit more important to have a high performance when the other players serve."),
              h3("Under Pressure Performance", class = "main-title"),
              plotOutput("under_pressure_graph"),
              p("Once again the Nadal's average performance is better than the rest of the players. This metric is a little bit tricky because it could be lower if the athlete played very well and didn't give chances to break his service. This graph by itself doesn't give much information, but it becomes very interesting if it's linked to the serve plot."),
              p("On one hand, there is Australian Open and Shangai scenario. In both tourneys, it's very easy to see how the service performance decreases when the matches go by but -  incredibly-  his under pressure performance improves. So, what is happening? As the minutes played increase, the fatigue makes Rafa perform worse in his service, which leads to give more chances to has his service broken. But in those cases, Nadal shows how well player he is by winning these hard points."),
              p("On the other hand, there is Roland Garros, where this performance is much lower than it is expected to. This is a case where Rafa won this tournament without any effort, so he didn't go through pressure moments."),
              p("Finally, it can be observed that in several tourneys where he reached the final and lost, this metric is worse in the final than in the semi-final."),
              br(),
              p("In conclusion, I would not say that playing tennis is 80% mind, neither that being a very skilled player is not important. But I'd say that the difference between being just a very good player and becoming one of the best in the history of this sport, lies in having a very strong mind that allows the athlete to draw his best shots when the circumstances demand it.")
              )
      #End Graph Tab ----
    )
    #End Tab Items ----
  )
  #End Dashboard Page
)
######## END SHINY UI #####