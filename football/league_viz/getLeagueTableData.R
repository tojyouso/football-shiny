# load libraries

library(tidyverse)
library(lubridate)

# create seasons to download
seas <- 2017:1995
leagues <- c("E0", "E1", "SC0", "D1", "I1", "SP1", "F1", "N1",
             "B1", "P1", "T1", "G1")

df <- data_frame()

for(j in leagues) {
  for (i in seas) {
    
    df_tmp <- read_csv(str_c("http://football-data.co.uk/mmz4281/", 
                             str_sub(as.numeric(i), 3, 4),
                             str_sub(as.numeric(i) + 1, 3, 4),
                             "/",
                             j, 
                             ".csv", sep = "")) %>% 
      # mutate(match_id = group_indices()) %>% 
      select(Date, HomeTeam, AwayTeam, FTHG, FTAG) %>% 
      # mutate(match_id = group_indices(Date, HomeTeam, AwayTeam)) %>% 
      mutate(season = i,
             league = j)
    
    df <- bind_rows(df, df_tmp)
    
    print(i)
  }
  
  print(j)
}


# split each game into the persective of each team
played_games <- df %>% 
  # select(match_id, HomeTeam, AwayTeam, FTHG, FTAG) %>% 
  rename(team = HomeTeam,
         opponent = AwayTeam,
         goals_scored = FTHG,
         goals_conceded = FTAG) %>% 
  mutate(home = 1) %>% 
  bind_rows(df %>% 
              # select(match_id, HomeTeam, AwayTeam, FTHG, FTAG) %>% 
              rename(opponent = HomeTeam,
                     team = AwayTeam,
                     goals_scored = FTAG,
                     goals_conceded = FTHG) %>% 
              mutate(home = 0)) %>% 
  # arrange(match_id) %>% 
  rename(date = Date) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(country = case_when(
    league == "E0" ~ "England",
    league == "E1" ~ "England (Championship)",
    league == "SC0" ~ "Scotland",
    league == "D1" ~ "Germany",
    league == "I1" ~ "Italy",
    league == "SP1" ~ "Spain",
    league == "F1" ~ "France",
    league == "N1" ~ "Netherlands",
    league == "B1" ~ "Belgium",
    league == "P1" ~ "Portugal",
    league == "T1" ~ "Turkey",
    league == "G1" ~ "Greece"
    
  ))

write_csv(played_games,
          "/Users/mobolayo/Documents/R/shiny-server/football/league_viz/played_games.csv")

write_csv(played_games %>% 
            filter(country == "England"),
          "/Users/mobolayo/Documents/R/shiny-server/football/league_proj_viz/played_games.csv")
