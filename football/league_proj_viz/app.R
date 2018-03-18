#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# add home and away
# add other leagues

library(tidyverse)
# library(stringr)
library(lubridate)
# library(forcats)
library(shiny)
library(shinythemes)
# library(DT)

played_games <- read_csv("played_games.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
  
  # Application title
  titlePanel("Projected totals by season"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # selectInput("land",
      #             "Which country",
      #             choices = sort(c("England", "England (Championship)", "Scotland",
      #                              "Germany", "Italy", "Spain", "France",
      #                              "Netherlands", "Belgium", "Portugal",
      #                              "Turkey", "Greece")),
      #             selected = "England"),
      
      selectInput("team",
                  "What team:",
                  choices = sort(unique(played_games$team)),
                  selected = "Arsenal"),
      
      selectInput("season",
                  "What year did the season start:",
                  choices = 2017:1995,
                  selected = 2017),
      
      selectInput("metric",
                  "What metric:",
                  choices = c("Points", "Goals scored", "Goals conceded",
                              "Goal difference"),
                  selected = "Points"),
      
      sliderInput("gameweek",
                  "At what gameweek:",
                  min = 1,
                  max = 38,
                  step = 1,
                  animate = T,
                  value = 29),
      
      p("This app is a visual representation of the projected total points to be achieved by
        a selected team this season vs previous seasons since 1995/1996"),
      
      br(),
      
      p("The projections are made based on the total number of games to be played in the
        latest season (2017)."),
      
      # br(),
      # 
      # p("The app allows you to choose which country and season you want to focus on and you are able to change 
      #   how the teams are ranked based on different metrics"),
      
      br(),
      
      a("Find me @tojyouso", href = "https://twitter.com/TojYouSo", target = "_blank"),
      br(),
      a("Source: http://football-data.co.uk as at 2018-03-16", href = "http://www.football-data.co.uk", target = "_blank")
      
      
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
  
  
  # get base data
  team_plots <- reactive({
    
    
    played_games %>% 
      # filter(team == "Man United") %>% 
      group_by(team, season) %>% 
      arrange(date) %>% 
      # select(Date, date, season, team, opponent) %>% 
      mutate(points = 3 * (goals_scored > goals_conceded) + (goals_scored == goals_conceded),
             points = case_when(
        input$metric == "Points" ~ points,
        # force case when to all be doubles not integers
        input$metric == "Goals scored" ~ as.numeric(goals_scored),
        input$metric == "Goals conceded" ~ as.numeric(goals_conceded),
        input$metric == "Goal difference" ~ as.numeric(goals_scored - goals_conceded)
      )) %>% 
      mutate(gameweek = row_number(),
             points = cumsum(points))
    
  })
  
  
  # get background data
  team_plot_bg <- reactive({
    
    team_plots() %>% 
      filter(season != input$season,
             season < 2017,
             team == input$team)
  })
  
  # get in focus data
  team_plot_fg <- reactive({
    
    team_plots() %>% 
      filter(season == input$season,
             team == input$team) %>%
      filter(gameweek <= input$gameweek)
  })
  
  # get projected data
  team_plot_proj <- reactive({
    
    team_plots() %>% 
      filter(season == input$season,
             team == input$team,
             gameweek <= input$gameweek) %>% 
      filter(gameweek == max(gameweek)) %>% 
      bind_rows(team_plots() %>% 
                  filter(season == input$season,
                         team == input$team,
                         gameweek <= input$gameweek) %>% 
                  filter(gameweek == max(gameweek)) %>% 
                  mutate(points = round(38 * points / gameweek, 0),
                         gameweek = 38))
    
  })
  
  output$league_plot <- renderPlot({
    
    ggplot(data = team_plot_bg(),
           aes(x = gameweek, y = points, group = season)) +
      geom_line(colour = "#333333") +
      theme_minimal() +
      geom_line(data = team_plot_fg(),
                aes(x = gameweek, y = points, label = season), colour = "red") +
      theme(plot.background = element_rect(fill = "black"),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(colour = "white", size = rel(1.5), face = "italic"),
            plot.subtitle = element_text(colour = "white", face = "italic"),
            plot.caption = element_text(colour = "white", face = "italic")) +
      # add a dashed line to show where they will end uo
      geom_line(data = team_plot_proj(),
                aes(x = gameweek, y = points), colour = "red", linetype = "dashed") +
      geom_text(data = team_plot_proj() %>%
                  filter(gameweek == 38),
                aes(x = gameweek, y = points, label = points), colour = "white",
                nudge_x = 1, nudge_y = 0.9) 
    
    
    # +
      # labs(title = str_c("Projected league ", input$metric, "for ", input$team),
      #      subtitle = str_c("assuming current run rate\n", input$season, "/", input$seas + 1),
      #      caption = "as at 2018-03-09\n@tojyouso")
  })
  
  output$league_table <- renderTable({
    # team_plot_proj() %>%
    #   filter(gameweek == 38)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

