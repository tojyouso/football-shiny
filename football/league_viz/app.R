#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# sliding function for which years to include
# radio button to determine whether to use max/min or interquartile range
# table so people can look at it
# filter which gameweek? nah!
# change season start
# add table of results too
# add error catcher


library(tidyverse)
# library(stringr)
library(lubridate)
# library(forcats)
library(shiny)
# library(DT)

played_games <- read_csv("played_games.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualising league tables"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("land",
                  "Which country",
                  choices = sort(c("England", "England (Championship)", "Scotland",
                                   "Germany", "Italy", "Spain", "France",
                                   "Netherlands", "Belgium", "Portugal",
                                   "Turkey", "Greece")),
                  selected = "England"),
      
      selectInput("season",
                  "What year did the season start:",
                  choices = 2017:1995,
                  selected = 2017),
      selectInput("metric",
                  "What metric:",
                  choices = c("Points", "Goals Scored", "Goals Conceded",
                              "Goal Difference"),
                  selected = "Points"),
      
      p("This app is a visual representation of the projected total points to be achieved by each team in the top league of selected countries
        - represented by the red circles. It also shows the min, max and average points of teams that finished
        in the same position from the 1995/1996 season to the 2016/2017 season"),
      
      br(),
      
      p("The projections are made based on the total number of games to be played in the
        latest season (2017). If the app crashes then that season for that country is not in the data."),
      
      br(),
      
      p("The app allows you to choose which country and season you want to focus on and you are able to change 
        how the teams are ranked based on different metrics"),
      
      br(),
      
      a("Find me @tojyouso", href = "https://twitter.com/TojYouSo", target = "_blank"),
      br(),
      a("Source: http://football-data.co.uk as at 2018-03-09", href = "https://fantasy.premierleague.com", target = "_blank")
      
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("league_plot", height = "600px"),
      tableOutput("league_table"),
      width = 7
    )
      )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  all_seasons_positions <- reactive({
    
    
    played_games %>% 
      mutate(points = 3 * (goals_scored > goals_conceded) + (goals_scored == goals_conceded),
             num_games = 1) %>% 
      filter(country == input$land) %>% 
      drop_na() %>% 
      group_by(country, season, team) %>% 
      summarise_at(vars(num_games, goals_scored, goals_conceded, points), funs(sum)) %>% 
      # to get num of games played in recent seasons
      left_join(played_games %>% 
                  filter(season == 2016) %>% 
                  group_by(country, team) %>% 
                  summarise(num_games_played = n()) %>% 
                  distinct(country, num_games_played),
                "country") %>% 
      mutate_at(vars(goals_scored, goals_conceded, points),
                funs(num_games_played * . / num_games)) %>% 
      mutate(points = case_when(
        input$metric == "Points" ~ points,
        input$metric == "Goals Scored" ~ goals_scored,
        input$metric == "Goals Conceded" ~ goals_conceded,
        input$metric == "Goal Difference" ~ goals_scored - goals_conceded
      )) %>% 
      arrange(desc(points)) %>% 
      # have to create a way to separate the positions, first by goal diff then by goals scored
      mutate(position = min_rank(desc(points + 1/1000 * (goals_scored - goals_conceded) + 1/10000 * goals_scored ))) %>% 
      arrange(desc(season)) %>% 
      ungroup() 
    
  })
  
  max_min_points <- reactive({
    
    all_seasons_positions() %>% 
      filter(season < 2017,
             season > 1994) %>% 
      group_by(position) %>% 
      summarise(max_points = max(points),
                min_points = min(points),
                average_points = quantile(points, 0.5)) %>% 
      mutate(position = fct_reorder(as.character(position), -position))
    
  })
  
  
  current_season <- reactive({
    
    all_seasons_positions() %>% 
      filter(season == input$season) %>% 
      mutate(position = fct_reorder(as.character(position), -position))
  })
  
  
  
  
  output$league_plot <- renderPlot({
    
    
    max_min_points() %>%
      left_join(current_season() %>% 
                  mutate(current_position = as.numeric(as.character(position))),
                "position") %>% 
      mutate(team = fct_reorder(team, -current_position)) %>% 
      drop_na() %>% 
      ggplot() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            # panel.grid.major.x = element_line(size = 0.1),
            axis.text = element_blank(),
            plot.title = element_text(size = rel(2), face = "italic"),
            plot.subtitle = element_text(face = "italic"),
            plot.caption = element_text(face = "italic")) +
      geom_point(aes(y = team, x = max_points), colour = "black", size = 1, alpha = 0.4)+
      geom_point(aes(x = min_points, y = team), colour = "black", size = 1, alpha = 0.4) +
      geom_point(aes(x = average_points, y = team), colour = "black", size = 1, alpha = 0.4) +
      geom_segment(aes(yend = team, y = team, x = min_points, xend = max_points), alpha = 0.3) +
      # geom_bar(stat = "identity", width = 0.01) +
      geom_point(aes(x = points, y = team), colour = "red", size = 9, alpha = 0.4) +
      # theme_minimal() +
      geom_label(aes(x = pmin(points, min_points) - 11,
                     y = team,
                     label = str_c(current_position, ". " , team)),
                 label.size  = 0, size = 4) +
      # geom_text(label = "If EPL teams continued at their current\n run rate, Man City will set a new EPL points record", x = 20, y = 17, size = 3, fontface = "italic") +
      geom_text(aes(x = points, y = team, label = round(points, 0)),
                colour = "black", size = 3.5, fontface = "bold") +
      # scale_x_continuous(breaks = c(25, 50, 75, 100),
      #                    labels = c(25, "", "",  100)) +
      labs(title = NULL,
           subtitle = NULL,
           caption = NULL,
           y = NULL,
           x = NULL)
    
  })
  
  output$league_table <- renderTable({
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

