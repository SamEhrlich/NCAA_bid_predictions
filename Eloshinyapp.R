#read in cleaned half season data and then request new data from 2023 each week -- clean that data and rbind to current csv
suppressPackageStartupMessages({
  suppressWarnings({
    library(tidyverse)
    library(baseballr)
    library(elo)
    library(broom)
    library(lubridate)
    library(mgsub)
    library(rvest)
    library(caret)
    library(xgboost)
    library(tidyverse)
    library(shiny)
    library(shinythemes)
    library(shinyWidgets)
  })
})

#read in updated csv 
clean_data <- read.csv('/Users/sam/Desktop/MizBaseball/Playoff Projection/CleanHalfSeason1822.csv')

#maybe actual schedule has unique match ids that can be joined to clean data?
mizzou_sched <- read.csv('/Users/sam/Desktop/MizBaseball/Playoff Projection/Mizzou23Sched.csv')

bid_pred_xgb <- readRDS(file.path('/Users/sam/Desktop/MizBaseball/Playoff Projection/bid_pred_xgb.rds'))

miz_sched_fake <- dplyr::slice(mizzou_sched, 1:5)

ui <- fluidPage(
    
  
  sidebarPanel( 
    h3('Select Winner of Game'),
    width = 3,
  switchInput(inputId = "game1",
              onLabel = 'Win', offLabel = 'Lose',
              label = "Oklahoma St. Vs. Missouri Game 1:",
              value = TRUE),
  switchInput(inputId = "game2",
              onLabel = 'Win', offLabel = 'Lose',
              label = "TCU Vs. Missouri Game 1:",
              value = TRUE),
  switchInput(inputId = "game3",
              onLabel = 'Win', offLabel = 'Lose',
              label = "Texas Vs. Missouri Game 1:",
              value = TRUE),
  switchInput(inputId = "game4",
              onLabel = 'Win', offLabel = 'Lose',
              label = "FIU Vs. Missouri Game 1:",
              value = TRUE),
  switchInput(inputId = "game5",
              onLabel = 'Win', offLabel = 'Lose',
              label = "FIU Vs. Missouri Game 2:",
              value = TRUE)
  ),
  
 mainPanel(
   h4('Bid Prediction'),
   tableOutput('pred_bid'),
   
   width = 9,
   h1('Results'),
  # actionButton("submitbutton", "Submit", class = "btn btn-primary"),
  tableOutput('miz_sched')
  )
)

server <- function(input, output, session) {
  
  game_output <- reactive({
    paste(c(input$game1, input$game2, input$game3, input$game4, input$game5))
  })
  
  # observeEvent(
  #   input$submitbutton,
  #   game_output <- reactive({
  #     paste(c(input$game1, input$game2))
  #   })
  # )
  
  miz_sch <- reactive({
    
    dat <- miz_sched_fake %>%
      mutate(home_win = game_output(),
             home_win = ifelse(game_output() == 'TRUE' & away_team == 'Missouri',0, ifelse(
               game_output() == 'FALSE' & home_team == 'Missouri',0,1)),
             date = as.Date(date))
  })
  
  # output$miz_sched <- renderTable(miz_sch())
  
  full_clean_data <- reactive({
    
    joined_dat <- rbind(clean_data %>% mutate(date = as.Date(date, format = "%m/%d/%Y")), miz_sch())
    
    elo_optim <- elo.run(home_win ~ adjust(home_team, 34) + away_team,
            k = 3,
            data = joined_dat)
    
    elo_history_fake <- elo_optim %>%
      as.data.frame() %>%
      rename('home_team' = 1, 'away_team' = 2, 'prob_home_win' = 3, 'home_win' = 4, 'home_elo_change' = 5,
             'away_elo_change' = 6, 'home_updated_elo' = 7, 'away_updated_elo' = 8)
    
    joined_dat <- joined_dat %>%
      mutate(home_elo_before_game = elo_history_fake$home_updated_elo - elo_history_fake$home_elo_change,
             away_elo_before_game = elo_history_fake$away_updated_elo - elo_history_fake$away_elo_change,
             home_win_prob = elo_history_fake$prob_home_win,
             away_win_prob = 1 - home_win_prob,
             home_elo_after_game = elo_history_fake$home_updated_elo,
             away_elo_after_game = elo_history_fake$away_updated_elo,
             home_elo_change = elo_history_fake$home_elo_change,
             away_elo_change = elo_history_fake$away_elo_change)
    
    elo_changes_fake <- rbind(
      joined_dat %>%
        select(date, match_id, school = home_team, opponent = away_team, result = home_win, 
               school_elo_before_game = home_elo_before_game, 
               school_win_prob = home_win_prob, school_elo_change = home_elo_change, 
               school_elo_after_game = home_elo_after_game, conference = home_conference,
               conf_1 = conference_group_1_home, conf_2 = conference_group_2_home, 
               conf_3 = conference_group_3_home,last_year_bid = last_year_bid_home, year),
      joined_dat %>%  #creating the inverse to display the opponent as the school
        select(date, match_id, school = away_team, opponent = home_team, result = home_win, 
               school_elo_before_game = away_elo_before_game, 
               school_win_prob = away_win_prob, school_elo_change = away_elo_change, 
               school_elo_after_game = away_elo_after_game, conference = away_conference,
               conf_1 = conference_group_1_away, conf_2 = conference_group_2_away, 
               conf_3 = conference_group_3_away, last_year_bid = last_year_bid_away, year) %>%
        mutate(result = case_when(
          result == 1 ~ 0,
          result == 0 ~ 1,
          TRUE ~ .5
        )))
    
    elo_changes_fake <- elo_changes_fake %>%
      mutate(date = as.Date(date, format="%m/%d/%Y"),
             year = year(date)) %>%
      group_by(school) %>%
      arrange(school,date) %>%
      mutate(game_count = seq_along(school)) %>%
      ungroup() %>%
      group_by(school,year) %>%
      mutate(game_count_by_year = seq_along(school)) %>%
      ungroup() %>%
      group_by(school,year) %>%
      mutate(win_count = cumsum(result),
             rolling_win_pct = win_count/game_count_by_year) %>%
      ungroup()
    
    fake_test <- elo_changes_fake %>%
      filter(year == 2023, school == 'Missouri') %>%
      select(game_count_by_year, school, opponent, school_win_prob, result, school_elo_before_game, school_elo_after_game,
             rolling_win_pct, conf_1, conf_2, conf_3, last_year_bid)
    
    fake_matrix <- data.matrix(fake_test %>% select(-c(school, opponent, school_win_prob, result)))
    
    fake_xgb_matrix <- xgb.DMatrix(data = fake_matrix)
    
    fake_preds_xgb <- predict(bid_pred_xgb, newdata = fake_xgb_matrix, reshape = TRUE)
    
    fake_test$fake_pred_bid <- fake_preds_xgb
    
    fake_result_display <- fake_test %>%
      select(game_count_by_year, school, opponent, school_win_prob, fake_pred_bid, result)
    
  })
  
  output$miz_sched <- renderTable(full_clean_data())
  
  output$pred_bid <- renderTable(full_clean_data() %>% select(fake_pred_bid) %>% dplyr::slice(n()))
  
  
}

shinyApp(ui, server)


