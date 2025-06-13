#this script will download the season data from baseballr 

#begin by loading in packages and the csv for 2018-2022 that is already clean
suppressPackageStartupMessages({
  suppressWarnings({
    library(tidyverse)
    library(baseballr)
    library(elo)
    library(broom)
    library(lubridate)
    library(mgsub)
    library(rvest)
    library(dplyr)
    library(httr)
  })
})

# order of data collection -> get all d1 team names -> using ncaa_teams function for years 2021-2025 
# get team ids using function ncaa_school_id_lu
# scrape schedule info  
# clean up the schedule info
# tune and run elo model
# simulate tournament
# plots

######## get team names

d1_schools <- data.frame()
years <- c(2021,2022,2023,2024,2025)
for (year in years){
  cat("Collecting D1 schools for", year, "\n")
  schools <- ncaa_teams(year = year, division = 1)
  d1_schools <- rbind(d1_schools, schools)
  cat("Added", nrow(schools), "schools for", year, "\n")
}

#store this data to not have to run again
# write.csv(d1_schools, "d1_schools2125_raw.csv")

head(d1_schools)


###### get ids

school_name_mapping <- c(
  "Miami (FL)" = "Miami",
  "St. John's (NY)" = "St. John's", 
  "Miami (OH)" = "Miami",
  "LMU (CA)" = "LMU",
  "Saint Mary's (CA)" = "Saint Mary's",
  "St. Thomas (MN)" = "St. Thomas",
  "Queens (NC)" = "Queens"
)

#make a column with cleaned up abbreviations
d1_schools <- d1_schools %>%
  mutate(cleaned_school_name = ifelse(team_name %in% names(school_name_mapping),
                                      school_name_mapping[team_name],
                                      team_name))

school_ids_by_year <- data.frame()
unique_cleaned_schools <- unique(d1_schools$cleaned_school_name)

for (i in seq_along(unique_cleaned_schools)) {
  u_school <- unique_cleaned_schools[i]
  tryCatch({
    school_id <- ncaa_school_id_lu(team_name = u_school)
    school_ids_by_year <- rbind(school_ids_by_year, school_id)
    cat("Added", u_school, "(", i, "/", length(unique_cleaned_schools), ")\n")
  }, error = function(e) {
    cat("Error getting ID for", u_school, ":", e$message, "\n")
  })
}

#FDU is only mismatch changing from Fairleigh Dickinson in 2024 - manually fix this 
all_cols <- colnames(school_ids_by_year)
fdu_manual_entries <- data.frame(
  team_id = c(222, 222, 222),
  team_name = c("FDU", "FDU", "FDU"),
  conference = c("NEC", "NEC", "NEC"),
  conference_id = c(846, 846, 846),
  division = c(1, 1, 1),
  year = c(2021, 2022, 2023),
  season_id = c(15580, 15860, 16340)
)

# Add any missing columns as NA
missing_cols <- setdiff(all_cols, colnames(fdu_manual_entries))
for(col in missing_cols) {
  fdu_manual_entries[[col]] <- NA
}

# Reorder columns to match school_ids_by_year
fdu_manual_entries <- fdu_manual_entries[all_cols]

# Add the manual FDU entries to school_ids_by_year
school_ids_by_year <- rbind(school_ids_by_year, fdu_manual_entries)

#write school ids to csv
# write.csv(school_ids_by_year, "d1_schools2125ids_raw.csv", row.names = FALSE)

#join ids to schools 
d1_school_info <- d1_schools %>%
  select(team_name,conference_id,team_url, division,year) %>%
  left_join(school_ids_by_year %>%
              select(team_id,team_name, conference, division, year, season_id), 
            by = c("team_name","division","year"))


#get schedules of all teams for each year
missing_ids <- d1_school_info %>%
  filter(is.na(team_id) & year != 2025)
missing_ids_unique <- unique(missing_ids$team_name)
cat("Missing IDs:", length(missing_ids_unique), "\n")

ncaa_school_id_lu(team_name = "Fairleigh Dickinson")


d1_school_info %>%
  filter(division == 1 & team_name == 'Abilene Christian')

d1_school_info %>%
  filter(division == 1 & conference_id  == '827')

school_ids_by_year

#### 2025 teams team_id and conference mapping

school_id_map <- d1_school_info  %>%
  filter(!is.na(team_id)) %>%
  group_by(team_name) %>%
  slice(1) %>%
  select(team_name, team_id)

conference_map <- d1_school_info  %>%
  filter(!is.na(conference)) %>%
  group_by(conference_id) %>%
  slice(1) %>%
  select(conference, conference_id)

#all data up to 2024
d1_school_24 <- d1_school_info %>%
  filter(year != 2025)

d1_school_25 <- d1_school_info %>%
  filter(year == 2025)

#left join the maps to 2025 to get a complete 2025 dataset then rejoin all data together
d1_school_25_complete <- d1_school_25 %>%
  select(-c("team_id","conference")) %>%
  left_join(school_id_map, by = c("team_name")) %>%
  left_join(conference_map, by = c("conference_id"))

#bind pre25 and 25 back together
d1_school_info_clean <- rbind(d1_school_24,d1_school_25_complete)

miz_25 <- d1_school_info_clean %>%
  filter(team_id == 434 & year == 2025)

miz_25

#write cleaned data to csv
# write.csv(d1_school_info_clean, "d1_schools2125_clean_raw.csv", row.names = FALSE)


# Simple function to scrape raw schedule data
scrape_team_schedule_raw <- function(team_url, team_name = NULL) {
  base_url <- "https://stats.ncaa.org"
  full_url <- paste0(base_url, team_url)
  
  cat("Scraping:", team_name, "from", full_url, "\n")
  
  tryCatch({
    # Read the webpage
    page <- read_html(full_url)
    
    # Get all tables
    all_tables <- page %>% html_elements("table")
    
    if (length(all_tables) >= 1) {
      # Extract the first table (the schedule table)
      schedule_table <- all_tables[[1]] %>% html_table()
      
      cat("Found schedule table with", nrow(schedule_table), "rows\n")
      
      # Minimal cleaning - just remove empty rows and add team info
      schedule_clean <- schedule_table %>%
        filter(Date != "", Date != "Date") %>%  # Remove empty rows and header rows
        mutate(
          team_name = ifelse(is.null(team_name), "Unknown", team_name)
        )
      
      return(schedule_clean)
      
    } else {
      cat("No tables found on page\n")
      return(data.frame())
    }
    
  }, error = function(e) {
    cat("Error scraping", team_name, ":", e$message, "\n")
    return(data.frame())
  })
}

# Function to scrape multiple teams (raw data) - improved error handling
scrape_multiple_teams_raw <- function(teams_df) {
  cat("Starting to scrape", nrow(teams_df), "team(s)\n")
  cat("Estimated time: ~", round((nrow(teams_df) * 2.5) / 60, 1), "minutes\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  all_schedules <- list()
  failed_teams <- c()
  
  for (i in 1:nrow(teams_df)) {
    team_info <- teams_df[i, ]
    
    cat("[", i, "/", nrow(teams_df), "] Processing:", team_info$team_name, "\n")
    
    # Wrap the entire scraping attempt in error handling
    tryCatch({
      schedule <- scrape_team_schedule_raw(
        team_url = team_info$team_url,
        team_name = team_info$team_name
      )
      
      if (nrow(schedule) > 0) {
        # Add team metadata
        schedule$team_id <- team_info$team_id
        schedule$conference <- team_info$conference
        schedule$conference_id <- team_info$conference_id
        schedule$division <- team_info$division
        schedule$year <- team_info$year
        schedule$season_id <- team_info$season_id
        
        all_schedules[[i]] <- schedule
        cat("✓ Success:", nrow(schedule), "games scraped\n")
      } else {
        cat("⚠ Warning: No games found for", team_info$team_name, "\n")
        failed_teams <- c(failed_teams, paste0(team_info$team_name, " (no games)"))
      }
      
    }, error = function(e) {
      cat("✗ Error scraping", team_info$team_name, ":", e$message, "\n")
      failed_teams <<- c(failed_teams, paste0(team_info$team_name, " (", e$message, ")"))
    })
    
    # Progress update every 10 teams
    if (i %% 10 == 0) {
      successful <- length(all_schedules[!sapply(all_schedules, is.null)])
      cat("\n--- Progress Update ---\n")
      cat("Completed:", i, "/", nrow(teams_df), "teams\n")
      cat("Successful:", successful, "| Failed:", length(failed_teams), "\n")
      cat("Estimated time remaining:", round(((nrow(teams_df) - i) * 2.5) / 60, 1), "minutes\n")
      cat("----------------------\n\n")
    }
    
    # Add a random delay between 1-3 seconds to be respectful to the server
    sleep_time <- runif(1, min = 1, max = 3)
    if (i < nrow(teams_df)) {  # Don't sleep after the last team
      Sys.sleep(sleep_time)
    }
  }
  
  # Final summary
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("SCRAPING COMPLETE!\n")
  successful_schedules <- all_schedules[!sapply(all_schedules, is.null)]
  cat("Successfully scraped:", length(successful_schedules), "teams\n")
  cat("Failed to scrape:", length(failed_teams), "teams\n")
  
  if (length(failed_teams) > 0) {
    cat("\nFailed teams:\n")
    for (team in failed_teams) {
      cat("- ", team, "\n")
    }
  }
  
  # Combine all successful schedules
  if (length(successful_schedules) > 0) {
    final_schedule <- do.call(rbind, successful_schedules)
    cat("\nTotal games scraped:", nrow(final_schedule), "\n")
    cat("Teams with data:", length(unique(final_schedule$team_name)), "\n")
    return(final_schedule)
  } else {
    cat("\nNo schedules were successfully scraped\n")
    return(data.frame())
  }
}

# Function to clean the combined raw schedule data
clean_schedule_data <- function(raw_schedule_df, d1_schools_df = NULL) {
  cat("Cleaning schedule data...\n")
  
  cleaned_schedule <- raw_schedule_df %>%
    # Step 1: Handle doubleheader dates
    mutate(
      doubleheader_game = ifelse(str_detect(Date, "\\(1\\)$"), 1,
                                 ifelse(str_detect(Date, "\\(2\\)$"), 2, 0)),
      Date = str_remove(Date, "\\([12]\\)$") %>% str_trim()
    ) %>%
    # Step 2: Clean opponent and determine home/away
    mutate(
      is_away = str_detect(Opponent, "^@"),
      Opponent_raw = Opponent,  # Keep original for reference
      Opponent = str_remove(Opponent, "^@") %>% str_trim()
    ) %>%
    # Step 3: Check for neutral site games
    mutate(
      neutral_site = ifelse(str_detect(Opponent, "@"), 1, 0)
    ) %>%
    # Step 4: Clean opponent names (remove rankings and locations)
    mutate(
      opponent_clean = case_when(
        # Has ranking at start
        str_detect(Opponent, "^#\\d+\\s+") ~ {
          temp <- str_remove(Opponent, "^#\\d+\\s+")
          ifelse(str_detect(temp, "@"), 
                 str_extract(temp, "^[^@]+") %>% str_trim(),
                 temp %>% str_trim())
        },
        # Has @ but no ranking
        str_detect(Opponent, "@") ~ str_extract(Opponent, "^[^@]+") %>% str_trim(),
        # No ranking, no @
        TRUE ~ str_trim(Opponent)
      )
    ) %>%
    # Step 5: Extract game results and scores
    mutate(
      game_result = str_extract(Result, "^[WL]"),
      score_part = str_remove(Result, "^[WL]\\s*") %>% str_trim(),
      score_clean = str_remove(score_part, "\\s*\\([^)]*\\)") %>% str_trim()
    ) %>%
    # Separate scores
    tidyr::separate(score_clean, into = c("team_score", "opponent_score"), 
                    sep = "-", fill = "right", remove = FALSE) %>%
    # Step 6: Final assignments
    mutate(
      team_score = as.numeric(str_trim(team_score)),
      opponent_score = as.numeric(str_trim(opponent_score)),
      home_team_raw = ifelse(is_away, opponent_clean, team_name),
      away_team_raw = ifelse(is_away, team_name, opponent_clean),
      home_score = ifelse(is_away, opponent_score, team_score),
      away_score = ifelse(is_away, team_score, opponent_score)
    ) %>%
    # Step 7: Clean tournament names from team names
    mutate(
      home_team = str_replace(home_team_raw, "\\s+\\d{4}\\s+.*$", "") %>% str_trim(),
      away_team = str_replace(away_team_raw, "\\s+\\d{4}\\s+.*$", "") %>% str_trim()
    ) %>%
    select(
      Date,
      home_team,
      away_team,
      home_score,
      away_score,
      doubleheader_game,
      neutral_site,
      game_result,
      team_name,
      team_id,
      conference,
      conference_id,
      division,
      year,
      season_id,
      original_opponent = Opponent_raw,
      original_result = Result
    )
  
  # Step 8: Filter for D1 vs D1 games only
  if (!is.null(d1_schools_df)) {
    cat("Filtering for D1 vs D1 games only...\n")
    
    # Get list of D1 school names
    d1_school_names <- unique(d1_schools_df$team_name)
    cat("Found", length(d1_school_names), "unique D1 schools\n")
    
    # Filter games where both teams are D1
    d1_games <- cleaned_schedule %>%
      filter(
        home_team %in% d1_school_names & 
          away_team %in% d1_school_names
      )
    
    cat("Filtered from", nrow(cleaned_schedule), "to", nrow(d1_games), "D1 vs D1 games\n")
    cat("Removed", nrow(cleaned_schedule) - nrow(d1_games), "games against non-D1 teams\n")
    
    # Show some examples of what was filtered out
    non_d1_games <- cleaned_schedule %>%
      filter(!(home_team %in% d1_school_names & away_team %in% d1_school_names))
    
    if (nrow(non_d1_games) > 0) {
      cat("\nExamples of filtered out games:\n")
      sample_filtered <- non_d1_games %>%
        select(home_team, away_team) %>%
        distinct() %>%
        head(5)
      print(sample_filtered)
    }
    
    cleaned_schedule <- d1_games
  }
  
  cat("Cleaning complete! Final dataset has", nrow(cleaned_schedule), "games\n")
  return(cleaned_schedule)
}

raw_data_schedules <- scrape_multiple_teams_raw(d1_school_info_clean)
clean_data_schedules <- clean_schedule_data(raw_data_schedules, d1_school_info_clean)

# write.csv(clean_data_schedules, "d1_sched_raw.csv", row.names = FALSE)
<<<<<<< Updated upstream
=======
clean_data_schedules <- read.csv("d1_sched_raw.csv")
>>>>>>> Stashed changes

#remove games that did not result in a win or loss
clean_sched <- clean_data_schedules %>%
  filter(!is.na(game_result) & game_result %in% c("W","L"))

#remove ivy league 2021
clean_sched <- clean_sched %>%
  filter(!(year == 2021 & conference == 'Ivy League'))

#create unique identifier for each game to filter
clean_sched$unique_id <- paste0(clean_sched$home_team,'_',clean_sched$away_team,'_',clean_sched$home_score,'_',clean_sched$away_score,'_',clean_sched$doubleheader_game,'_',clean_sched$season_id)

#only want 1 instance of each game so slice top half
half_games_played <- clean_sched %>%
  group_by(unique_id) %>%
  dplyr::slice(1) %>%
  ungroup()

half_games_played$home_team_win <- ifelse(half_games_played$home_score > half_games_played$away_score, 1, 0)

# Order by date
half_games_played <- half_games_played %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%  
  arrange(Date)


# Create function to assign starting Elo by conference
get_starting_elo <- function(conference) {
  case_when(
    conference == "SEC" ~ 1500,
    conference == "ACC" ~ 1500,
    TRUE ~ 1500  # All other conferences
  )
}

# Create team-conference lookup
team_conference_lookup <- clean_sched %>%
  filter(year == 2025) %>%
  select(team_name, conference) %>%
  distinct()

# Create team_starting_elos vector with correct values
team_starting_elos <- team_conference_lookup %>%
  mutate(starting_elo = get_starting_elo(conference)) %>%
  select(team_name, starting_elo) %>%
  deframe()

# Get all teams from games and add any missing teams
teams_in_games <- unique(c(half_games_played$home_team, half_games_played$away_team))
missing_teams <- teams_in_games[!teams_in_games %in% names(team_starting_elos)]

if (length(missing_teams) > 0) {
  missing_elos <- rep(1500, length(missing_teams))
  names(missing_elos) <- missing_teams
  team_starting_elos <- c(team_starting_elos, missing_elos)
}

cat("Final team starting Elos count:", length(team_starting_elos), "\n")
cat("All teams covered:", all(teams_in_games %in% names(team_starting_elos)), "\n")

#set seed for reproducibility
set.seed(123)

# Elo model tuning using grid search
k.options <- seq(from = 10, to = 30, by = 1)
hfa.options <- seq(from = 30, to = 40, by = 1)
reg.options <- seq(from = 0.05, to = 0.20, by = 0.01)
grid = crossing(k = k.options, h = hfa.options, r = reg.options)

for (i in 1:nrow(grid)) {
  elo_optim <- elo.run(
    home_team_win ~ adjust(home_team, as.numeric(grid[i,'h'])) +
      neutral(neutral_site) + away_team + 
      regress(year, team_starting_elos, by = as.numeric(grid[i,'r'])),
    k = as.numeric(grid[i,'k']),
    initial.elos = team_starting_elos,
    data = half_games_played
  )
  
  grid[i,4] <- mse(elo_optim)
  grid[i,5] <- pROC::auc(elo_optim)
}

grid <- rename(grid, mse = ...4, auc = ...5)

# Get optimal parameters
optim_params <- grid %>%
  arrange(mse) %>%
  head(1)

# Run final Elo model with optimal parameters and correct starting Elo
elo_optim <- elo.run(
  home_team_win ~ adjust(home_team, as.numeric(optim_params$h)) +
    neutral(neutral_site) + away_team +
    regress(year, team_starting_elos, by = as.numeric(optim_params$r)),  # Low regression to maintain conference differences
  k = as.numeric(optim_params$k),
  initial.elos = team_starting_elos,  # This now has SEC=2000, ACC=1800
  data = half_games_played
)

# Get summary and final ratings
summary(elo_optim)
final.elos(elo_optim)

# Save final Elos for tournament simulation
team_ranks <- as.data.frame(final.elos(elo_optim))
team_ranks$school <- rownames(team_ranks)
rownames(team_ranks) <- 1:nrow(team_ranks)

# write.csv(team_ranks, "team_ranks.csv")

#### define the regionals while web scraping

##left side of bracket
#nashville regional (vanderbilt home)
nashville <- c("Vanderbilt", "Wright St.", "Louisville", "ETSU")
#hattiesburg regional (southern miss home)
hattiesburg <-c("Southern Miss.", "Columbia", "Miami (FL)", "Alabama")
#tallahassee regional (Florida St. home)
tallahassee <- c("Florida St.", "Bethune-Cookman", "Mississippi St.", "Northeastern")
#corvallis regional (Oregon St. home)
corvallis <- c("Oregon St.","Mount St. Mary's", "Southern California", "TCU")
#chapel hill regional (UNC home)
chapel_hill <- c("North Carolina","Holy Cross","Nebraska","Oklahoma")
#eugene regional (oregon home)
eugene <- c("Oregon","Utah Valley","Cal Poly","Arizona")
#conway regional (Coastal Carolina home)
conway <- c("Coastal Carolina","Fairfield","East Carolina","Florida")
#auburn regional (auburn home)
auburn <- c("Auburn","Central Conn. St.","Stetson","NC State")

## right side of bracket
#austin regional (Texas home)
austin <- c("Texas","Houston Christian","Kansas St.","UTSA")
#la regional (ucla home)
los_angeles <- c("UCLA","Fresno St.","UC Irvine","Arizona St.")
#oxford regional (ole miss home)
oxford <- c("Ole Miss","Murray St.","Georgia Tech","Western Ky.")
#athens regional (Georgia home)
athens <- c("Georgia","Binghamton","Duke","Oklahoma St.")
#baton rouge regional (LSU home)
baton_rouge <- c("LSU","Little Rock","DBU","Rhode Island")
#clemson regional (clemson home)
clemson <- c("Clemson","USC Upstate","West Virginia","Kentucky")
#knoxville regional (tennessee home)
knoxville <- c("Tennessee", "Miami (OH)", "Wake Forest", "Cincinnati")

#fayetteville regional (arkansas home)
fayetteville <- c("Arkansas","North Dakota St.","Creighton","Kansas")

regionals <- list(
  # Left side (top to bottom)
  nashville = nashville,
  hattiesburg = hattiesburg,
  tallahassee = tallahassee,
  corvallis = corvallis,
  chapel_hill = chapel_hill,
  eugene = eugene,
  conway = conway,
  auburn = auburn,
  # Right side (top to bottom)
  austin = austin,
  los_angeles = los_angeles,
  oxford = oxford,
  athens = athens,
  baton_rouge = baton_rouge,
  clemson = clemson,
  knoxville = knoxville,
  fayetteville = fayetteville
)

##### tourney sim

# Function to calculate win probability using elo package
calculate_win_prob <- function(elo1, elo2) {
  # Use the elo package's built-in probability function
  prob <- elo.prob(elo1, elo2)
  return(prob)
}

simulate_game <- function(team1, team2, team_ranks_df, verbose = FALSE) {
  # Get Elo ratings from your team_ranks dataframe
  elo1 <- team_ranks_df$`final.elos(elo_optim)`[team_ranks_df$school == team1]
  elo2 <- team_ranks_df$`final.elos(elo_optim)`[team_ranks_df$school == team2]
  
  # Handle missing teams
  if (length(elo1) == 0) {
    if (verbose) cat("Warning: No Elo found for", team1, "- using default 1500\n")
    elo1 <- 1500
  }
  if (length(elo2) == 0) {
    if (verbose) cat("Warning: No Elo found for", team2, "- using default 1500\n")
    elo2 <- 1500
  }
  
  # Calculate win probability for team1 using elo.prob
  win_prob <- elo.prob(elo1, elo2)
  
  # Simulate game
  winner <- ifelse(runif(1) < win_prob, team1, team2)
  
  if (verbose) {
    cat(sprintf("%s (%.0f) vs %s (%.0f) - Winner: %s (%.1f%% chance)\n", 
                team1, elo1, team2, elo2, winner, 
                ifelse(winner == team1, win_prob * 100, (1 - win_prob) * 100)))
  }
  
  return(winner)
}


# Function to simulate a double elimination regional tournament
simulate_regional <- function(teams, team_ranks_df, regional_name = "", verbose = FALSE) {
  if (verbose) cat("\n=== SIMULATING", toupper(regional_name), "REGIONAL ===\n")
  
  # Initial matchups: 1v4, 2v3
  game1_winner <- simulate_game(teams[1], teams[4], team_ranks_df, verbose)
  game2_winner <- simulate_game(teams[2], teams[3], team_ranks_df, verbose)
  
  # Losers bracket
  game1_loser <- ifelse(game1_winner == teams[1], teams[4], teams[1])
  game2_loser <- ifelse(game2_winner == teams[2], teams[3], teams[2])
  
  # Winners bracket final
  winners_final_winner <- simulate_game(game1_winner, game2_winner, team_ranks_df, verbose)
  winners_final_loser <- ifelse(winners_final_winner == game1_winner, game2_winner, game1_winner)
  
  # Losers bracket games
  losers_game1 <- simulate_game(game1_loser, game2_loser, team_ranks_df, verbose)
  losers_final <- simulate_game(losers_game1, winners_final_loser, team_ranks_df, verbose)
  
  # Championship game(s)
  champ_game1 <- simulate_game(winners_final_winner, losers_final, team_ranks_df, verbose)
  
  # If losers bracket team wins, they play again (double elimination)
  if (champ_game1 == losers_final) {
    if (verbose) cat("Losers bracket team won! Playing deciding game...\n")
    regional_winner <- simulate_game(winners_final_winner, losers_final, team_ranks_df, verbose)
  } else {
    regional_winner <- champ_game1
  }
  
  if (verbose) cat("Regional Winner:", regional_winner, "\n")
  return(regional_winner)
}

# Function to simulate all regionals
simulate_all_regionals <- function(team_ranks_df, verbose = FALSE) {
  regional_winners <- list()
  
  for (i in 1:length(regionals)) {
    regional_name <- names(regionals)[i]
    teams <- regionals[[i]]
    winner <- simulate_regional(teams, team_ranks_df, regional_name, verbose)
    regional_winners[[regional_name]] <- winner
  }
  
  return(regional_winners)
}

# Function to simulate super regionals
simulate_super_regionals <- function(regional_winners, team_ranks_df, verbose = FALSE) {
  if (verbose) cat("\n=== SUPER REGIONALS ===\n")
  
  # Convert to vector for easier indexing
  winners <- unlist(regional_winners)
  
  # Super regional matchups (based on bracket positioning)
  super_regional_matchups <- list(
    c(winners[1], winners[2]),   # nashville vs hattiesburg
    c(winners[3], winners[4]),   # tallahassee vs corvallis
    c(winners[5], winners[6]),   # chapel_hill vs eugene
    c(winners[7], winners[8]),   # conway vs auburn
    c(winners[9], winners[10]),  # austin vs los_angeles
    c(winners[11], winners[12]), # oxford vs athens
    c(winners[13], winners[14]), # baton_rouge vs clemson
    c(winners[15], winners[16])  # knoxville vs fayetteville
  )
  
  cws_teams <- c()
  
  for (i in 1:length(super_regional_matchups)) {
    matchup <- super_regional_matchups[[i]]
    # Best of 3 series (simulate as single game for simplicity, but could expand)
    winner <- simulate_game(matchup[1], matchup[2], team_ranks_df, verbose)
    cws_teams <- c(cws_teams, winner)
    
    if (verbose) {
      cat(sprintf("Super Regional %d: %s vs %s -> Winner: %s\n", 
                  i, matchup[1], matchup[2], winner))
    }
  }
  
  return(cws_teams)
}

# Function to simulate College World Series (double elimination)
simulate_cws <- function(cws_teams, team_ranks_df, verbose = FALSE) {
  if (verbose) cat("\n=== COLLEGE WORLD SERIES ===\n")
  
  # Initial bracket setup (8 teams, double elimination)
  # For simplicity, we'll simulate this as two 4-team double elimination brackets
  
  bracket1 <- cws_teams[1:4]
  bracket2 <- cws_teams[5:8]
  
  if (verbose) {
    cat("Bracket 1:", paste(bracket1, collapse = ", "), "\n")
    cat("Bracket 2:", paste(bracket2, collapse = ", "), "\n")
  }
  
  # Simulate each bracket (simplified double elimination)
  bracket1_winner <- simulate_regional(bracket1, team_ranks_df, "CWS Bracket 1", verbose)
  bracket2_winner <- simulate_regional(bracket2, team_ranks_df, "CWS Bracket 2", verbose)
  
  # Championship series (best of 3, simplified as best of 5)
  if (verbose) cat("\n=== CHAMPIONSHIP SERIES ===\n")
  
  # Simulate best of 3 (simplified as single game with higher weight)
  champion <- simulate_game(bracket1_winner, bracket2_winner, team_ranks_df, verbose)
  
  if (verbose) {
    cat(sprintf("\nCHAMPION: %s defeats %s\n", 
                champion, ifelse(champion == bracket1_winner, bracket2_winner, bracket1_winner)))
  }
  
  return(list(
    champion = champion,
    runner_up = ifelse(champion == bracket1_winner, bracket2_winner, bracket1_winner),
    cws_teams = cws_teams
  ))
}

# Main tournament simulation function
simulate_tournament <- function(team_ranks_df, verbose = FALSE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  if (verbose) cat("STARTING NCAA BASEBALL TOURNAMENT SIMULATION\n")
  if (verbose) cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # Step 1: Simulate regionals
  regional_winners <- simulate_all_regionals(team_ranks_df, verbose)
  
  # Step 2: Simulate super regionals  
  cws_teams <- simulate_super_regionals(regional_winners, team_ranks_df, verbose)
  
  # Step 3: Simulate College World Series
  results <- simulate_cws(cws_teams, team_ranks_df, verbose)
  
  # Return comprehensive results
  return(list(
    champion = results$champion,
    runner_up = results$runner_up,
    cws_teams = results$cws_teams,
    regional_winners = regional_winners,
    team_ranks_df = team_ranks_df
  ))
}

# Function to run multiple simulations
run_multiple_simulations <- function(team_ranks_df, n_sims = 1000, verbose = FALSE) {
  cat("Running", n_sims, "tournament simulations...\n")
  
  results <- list()
  champions <- c()
  
  for (i in 1:n_sims) {
    if (i %% 100 == 0) cat("Completed", i, "simulations\n")
    
    sim_result <- simulate_tournament(team_ranks_df, verbose = FALSE)
    results[[i]] <- sim_result
    champions <- c(champions, sim_result$champion)
  }
  
  # Calculate championship probabilities
  champ_probs <- table(champions) / n_sims * 100
  champ_probs <- sort(champ_probs, decreasing = TRUE)
  
  cat("\nChampionship Probabilities (%):\n")
  for (i in 1:min(10, length(champ_probs))) {
    cat(sprintf("%s: %.1f%%\n", names(champ_probs)[i], champ_probs[i]))
  }
  
  return(list(
    results = results,
    championship_probabilities = champ_probs,
    n_simulations = n_sims
  ))
}

#sim results
result <- simulate_tournament(team_ranks, verbose = TRUE)
sim_results <- run_multiple_simulations(team_ranks, n_sims = 5000)


#### get more detailed sims

run_multiple_simulations_detailed <- function(team_ranks_df, n_sims = 1000, verbose = FALSE) {
  cat("Running", n_sims, "detailed tournament simulations...\n")
  
  # Initialize tracking lists
  results <- list()
  champions <- c()
  all_regional_winners <- list()
  all_cws_teams <- list()
  all_super_regional_winners <- list()
  
  # Initialize regional winner tracking
  regional_names <- names(regionals)
  regional_winner_counts <- list()
  
  for (regional in regional_names) {
    regional_winner_counts[[regional]] <- c()
  }
  
  # Run simulations
  for (i in 1:n_sims) {
    if (i %% 100 == 0) cat("Completed", i, "simulations\n")
    
    sim_result <- simulate_tournament(team_ranks_df, verbose = FALSE)
    results[[i]] <- sim_result
    champions <- c(champions, sim_result$champion)
    
    # Track regional winners
    for (regional in regional_names) {
      winner <- sim_result$regional_winners[[regional]]
      regional_winner_counts[[regional]] <- c(regional_winner_counts[[regional]], winner)
    }
    
    # Track CWS teams and other results
    all_cws_teams[[i]] <- sim_result$cws_teams
  }
  
  # Calculate regional winner percentages
  regional_percentages <- list()
  
  for (regional in regional_names) {
    winner_table <- table(regional_winner_counts[[regional]])
    winner_percentages <- round((winner_table / n_sims) * 100, 1)
    regional_percentages[[regional]] <- sort(winner_percentages, decreasing = TRUE)
  }
  
  # Calculate championship probabilities
  champ_probs <- table(champions) / n_sims * 100
  champ_probs <- sort(champ_probs, decreasing = TRUE)
  
  # Calculate CWS probabilities
  cws_participants <- unlist(all_cws_teams)
  cws_probs <- table(cws_participants) / n_sims * 100
  cws_probs <- sort(cws_probs, decreasing = TRUE)
  
  # Display results
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("TOURNAMENT SIMULATION RESULTS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Regional Results
  cat("\nREGIONAL WINNER PROBABILITIES:\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  for (regional in regional_names) {
    cat(toupper(regional), "REGIONAL:\n")
    regional_teams <- regionals[[regional]]
    cat("Teams:", paste(regional_teams, collapse = ", "), "\n")
    
    for (j in 1:length(regional_percentages[[regional]])) {
      team <- names(regional_percentages[[regional]])[j]
      pct <- regional_percentages[[regional]][j]
      cat(sprintf("  %s: %.1f%%\n", team, pct))
    }
    cat("\n")
  }
  
  # CWS Probabilities
  cat("COLLEGE WORLD SERIES PROBABILITIES:\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  for (i in 1:min(15, length(cws_probs))) {
    cat(sprintf("%s: %.1f%%\n", names(cws_probs)[i], cws_probs[i]))
  }
  
  # Championship Probabilities
  cat("\nCHAMPIONSHIP PROBABILITIES:\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  for (i in 1:min(10, length(champ_probs))) {
    cat(sprintf("%s: %.1f%%\n", names(champ_probs)[i], champ_probs[i]))
  }
  
  return(list(
    results = results,
    regional_probabilities = regional_percentages,
    cws_probabilities = cws_probs,
    championship_probabilities = champ_probs,
    n_simulations = n_sims
  ))
}

# Function to create a bracket prediction based on simulation results
create_bracket_predictions <- function(simulation_results, confidence_threshold = 50) {
  cat("\nBRACKET PREDICTIONS (Most Likely Winners):\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  regional_predictions <- list()
  
  # Get most likely winner from each regional
  for (regional_name in names(simulation_results$regional_probabilities)) {
    regional_probs <- simulation_results$regional_probabilities[[regional_name]]
    most_likely_winner <- names(regional_probs)[1]
    win_percentage <- regional_probs[1]
    
    regional_predictions[[regional_name]] <- most_likely_winner
    
    confidence_level <- ifelse(win_percentage >= confidence_threshold, "HIGH", "LOW")
    
    cat(sprintf("%s: %s (%.1f%% - %s CONFIDENCE)\n", 
                toupper(regional_name), 
                most_likely_winner, 
                win_percentage, 
                confidence_level))
  }
  
  # Show predicted matchups for Super Regionals
  cat("\nPREDICTED SUPER REGIONAL MATCHUPS:\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")
  
  predicted_winners <- unlist(regional_predictions)
  
  super_regional_matchups <- list(
    c(predicted_winners[1], predicted_winners[2]),   # nashville vs hattiesburg
    c(predicted_winners[3], predicted_winners[4]),   # tallahassee vs corvallis
    c(predicted_winners[5], predicted_winners[6]),   # chapel_hill vs eugene
    c(predicted_winners[7], predicted_winners[8]),   # conway vs auburn
    c(predicted_winners[9], predicted_winners[10]),  # austin vs los_angeles
    c(predicted_winners[11], predicted_winners[12]), # oxford vs athens
    c(predicted_winners[13], predicted_winners[14]), # baton_rouge vs clemson
    c(predicted_winners[15], predicted_winners[16])  # knoxville vs fayetteville
  )
  
  for (i in 1:length(super_regional_matchups)) {
    matchup <- super_regional_matchups[[i]]
    cat(sprintf("Super Regional %d: %s vs %s\n", i, matchup[1], matchup[2]))
  }
  
  # Most likely champion
  top_champion <- names(simulation_results$championship_probabilities)[1]
  champion_pct <- simulation_results$championship_probabilities[1]
  
  cat(sprintf("\nMOST LIKELY CHAMPION: %s (%.1f%%)\n", top_champion, champion_pct))
  
  return(regional_predictions)
}

# Function to compare specific teams in any matchup
compare_teams <- function(team1, team2, team_ranks_df, n_sims = 1000) {
  cat(sprintf("Simulating %s vs %s (%d simulations):\n", team1, team2, n_sims))
  
  wins_team1 <- 0
  
  for (i in 1:n_sims) {
    winner <- simulate_game(team1, team2, team_ranks_df, verbose = FALSE)
    if (winner == team1) wins_team1 <- wins_team1 + 1
  }
  
  team1_pct <- round((wins_team1 / n_sims) * 100, 1)
  team2_pct <- round(100 - team1_pct, 1)
  
  cat(sprintf("%s: %.1f%%\n", team1, team1_pct))
  cat(sprintf("%s: %.1f%%\n", team2, team2_pct))
  
  return(list(
    team1 = team1,
    team2 = team2,
    team1_win_pct = team1_pct,
    team2_win_pct = team2_pct
  ))
}

#sim detailed tournament results
detailed_results <- run_multiple_simulations_detailed(team_ranks, n_sims = 5000)
bracket_predictions <- create_bracket_predictions(detailed_results)

champ_table <- data.frame(
  Team = names(detailed_results$championship_probabilities),
  Win_Probability = as.numeric(detailed_results$championship_probabilities)
) %>%
  mutate(
    Rank = row_number(),
    Win_Percentage = paste0(round(Win_Probability, 1), "%")
  ) %>%
  select(Rank, Team, Win_Percentage)

print(champ_table)

regional_table <- data.frame(
  Team = names(detailed_results$regional_probabilities$nashville),
  Regional_Win_Probability = as.numeric(detailed_results$regional_probabilities$nashville)
) %>%
  mutate(
    Rank = row_number(),
    Win_Percentage = paste0(round(Regional_Win_Probability, 1), "%")
  ) %>%
  select(Rank, Team, Win_Percentage)

print(regional_table)

detailed_results$regional_probabilities

##### plotting

elo_history <- as.data.frame(elo_optim)

#bind elo back to original df
games_with_elo <- bind_cols(half_games_played, elo_history)

#flip home away to team and opponent for full schedules
create_team_schedules <- function(games_with_elo) {
  
  # Create home team records
  home_records <- games_with_elo %>%
    mutate(
      team = home_team,
      opponent = away_team,
      team_score = home_score,
      opponent_score = away_score,
      is_home = 1,
      team_won = home_team_win,
      team_elo_after = elo.A,
      opponent_elo_after = elo.B,
      team_elo_change = update.A,
      opponent_elo_change = update.B,
      win_probability = p.A
    )
  
  # Create away team records  
  away_records <- games_with_elo %>%
    mutate(
      team = away_team,
      opponent = home_team,
      team_score = away_score,
      opponent_score = home_score,
      is_home = 0,
      team_won = 1 - home_team_win,  # Flip the win indicator
      team_elo_after = elo.B,
      opponent_elo_after = elo.A,
      team_elo_change = update.B,
      opponent_elo_change = update.A,
      win_probability = 1 - p.A  # Flip the probability
    )
  
  # Combine both records
  team_schedules <- bind_rows(home_records, away_records) %>%
    select(
      Date, year, team, opponent, 
      team_score, opponent_score, team_won,
      is_home, neutral_site, doubleheader_game,
      team_elo_after, team_elo_change, 
      opponent_elo_after, win_probability
    ) %>%
    arrange(team, Date)
  
  return(team_schedules)
}

team_schedules <- create_team_schedules(games_with_elo)

# write.csv(team_schedules, "team_schedules.csv", row.names = FALSE)

# function to get team sched
get_team_schedule <- function(team_name, year_filter = NULL) {
  
  schedule <- team_schedules %>%
    filter(team == team_name)
  
  if (!is.null(year_filter)) {
    schedule <- schedule %>% filter(year == year_filter)
  }
  
  schedule <- schedule %>%
    arrange(Date) %>%
    mutate(game_number = row_number())
  
  return(schedule)
}

# Extract regional hosts
regional_hosts <- c(
  "Vanderbilt",        # Nashville
  "Southern Miss.",    # Hattiesburg
  "Florida St.",       # Tallahassee
  "Oregon St.",        # Corvallis
  "North Carolina",    # Chapel Hill 
  "Oregon",           # Eugene
  "Coastal Carolina",  # Conway
  "Auburn",           # Auburn
  "Texas",            # Austin
  "UCLA",             # Los Angeles
  "Ole Miss",         # Oxford
  "Georgia",          # Athens
  "LSU",              # Baton Rouge
  "Clemson",          # Clemson
  "Tennessee",        # Knoxville
  "Arkansas"          # Fayetteville
)

#get 2025 regional hosts
regional_2025_data <- team_schedules %>%
  filter(team %in% regional_hosts & year == 2025) %>%
  arrange(team, Date) %>%
  group_by(team) %>%
  mutate(game_number = row_number()) %>%
  ungroup()

# Check for any missing teams
missing_hosts <- regional_hosts[!regional_hosts %in% unique(regional_2025_data$team)]
if (length(missing_hosts) > 0) {
  cat("Missing hosts:", paste(missing_hosts, collapse = ", "), "\n")
}

school_colors <- c(
  "Vanderbilt" = "#B8860B",        # Gold
  "Southern Miss." = "#FFD700",     # Gold  
  "Florida St." = "#782F40",        # Garnet
  "Oregon St." = "#FF6600",         # Orange
  "North Carolina" = "#4B9CD3",     # Carolina Blue
  "Oregon" = "#007030",             # Green
  "Costal Carolina" = "#006A4E",    # Teal
  "Auburn" = "#FF8C00",             # Orange
  "Texas" = "#BF5700",              # Burnt Orange
  "UCLA" = "#2774AE",               # UCLA Blue
  "Ole Miss" = "#CE1126",           # Red
  "Georgia" = "#BA0C2F",            # Red
  "LSU" = "#461D7C",                # Purple
  "Clemson" = "#F56600",            # Orange
  "Tennessee" = "#FF8200",          # Orange
  "Arkansas" = "#9D2235"            # Cardinal
)

# Create improved plot
regional_plot <- ggplot(regional_2025_data, aes(x = Date, y = team_elo_after, color = team)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 0.8, alpha = 0.7) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),  
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line()
  ) +
  labs(
    title = "NCAA Baseball Regional Hosts - 2025 Elo Trajectories",
    x = "Date",
    y = "Elo Rating", 
    color = "Regional Host"
  ) +
  scale_x_date(
    date_labels = "%b %d", 
    date_breaks = "2 weeks",
    expand = expansion(mult = c(0.02, 0.02))  
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.02)),  
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_color_manual(values = school_colors) +
  geom_hline(yintercept = 1500, linetype = "dashed", alpha = 0.6, color = "gray50", size = 0.8) +
  coord_cartesian(clip = "off")

print(regional_plot)


final_elo_rating_table <- team_ranks %>%
  rename(`Elo Rating` = `final.elos(elo_optim)`,
         Team = school) %>%
  arrange(desc(`Elo Rating`)) %>%
  mutate(Rank = row_number())


#save items to reproduce quickly
final_elo_rating_table <- team_ranks %>%
  rename(`Elo Rating` = `final.elos(elo_optim)`) %>%
  arrange(desc(`Elo Rating`)) %>%
  mutate(Rank = row_number())

write.csv(optim_params, "optimal_elo_params.csv")
write.csv(final_elo_rating_table, "final_elo_rating_table.csv")

iowa24_25 <- team_schedules %>%
  filter(team == "Iowa" & year %in% c(2024, 2025))

# Create improved plot
iowa_plot <- ggplot(iowa24_25, aes(x = Date, y = team_elo_after)) +
  geom_line(linewidth = 1.2, alpha = 0.9, color = "#FFD700") +  # Move color here
  geom_point(size = 1, color = "#FFD700") +  # Move color here
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),  
    legend.position = "none",  # Remove legend since we're not using color mapping
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(),
    strip.text = element_text(size = 14, face = "bold")  # Style the facet labels
  ) +
  labs(
    title = "Iowa ELO Progression: 2024-2025",
    x = "Date",
    y = "ELO Rating"
  ) +
  scale_x_date(
    date_labels = "%b", 
    date_breaks = "1 month",
    expand = expansion(mult = c(0.02, 0.02))  
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.02)),  
    breaks = scales::pretty_breaks(n = 6)
  ) +
  geom_hline(yintercept = 1500, linetype = "dashed", alpha = 0.6, color = "gray50", size = 0.8) +
  coord_cartesian(clip = "off") +
  facet_wrap(~ year, scales = "free_x")  # Use facet_wrap for side-by-side panels

print(iowa_plot)


final_elos <- read.csv("final_elo_rating_table.csv")
top8_teams <- c("Oregon St.", "Coastal Carolina", "Arizona", "Louisville", 
                "Arkansas", "UCLA", "LSU", "Murray St.")
top8 <- final_elos %>%
  filter(school %in% top8_teams) %>%
  rename(`Elo Rating` = "Elo.Rating")

top8_ranks <- final_elos %>%
  filter(school %in% top8_teams)

cat("Top 8 Teams Final Elo Rankings:\n")
cat("================================\n")

# Filter top8_ranks for only the teams that made it to CWS
top8_elo_data <- top8_ranks %>%
  filter(school %in% top8_teams) %>%
  rename(team = school, elo_rating = Elo.Rating, original_rank = Rank) %>%
  arrange(original_rank)

# Display the rankings
for (i in 1:nrow(top8_elo_data)) {
  cat(sprintf("%s: Rank %d, Elo %.0f\n", 
              top8_elo_data$team[i], 
              top8_elo_data$original_rank[i], 
              top8_elo_data$elo_rating[i]))
}

cat("\nTop 8 Teams Sorted by Original Elo Ranking:\n")
cat("==========================================\n")
for (i in 1:nrow(top8_elo_data)) {
  cat(sprintf("%d. %s (%.0f)\n", 
              top8_elo_data$original_rank[i], 
              top8_elo_data$team[i], 
              top8_elo_data$elo_rating[i]))
}

# Create team_ranks dataframe for top 8 only (matching your original format)
top8_team_ranks <- data.frame(
  school = top8_elo_data$team,
  elo_rating = top8_elo_data$elo_rating
)
names(top8_team_ranks)[2] <- "final.elos(elo_optim)"

cws_bracket <- list(
  game1 = c("Coastal Carolina", "Arizona"),
  game2 = c("Oregon St.", "Louisville"), 
  game3 = c("UCLA", "Murray St."),
  game4 = c("Arkansas", "LSU")
)

# Function to simulate CWS with the actual bracket structure
simulate_cws_top8 <- function(team_ranks_df, bracket, verbose = FALSE) {
  if (verbose) cat("\n=== COLLEGE WORLD SERIES TOP 8 SIMULATION ===\n")
  
  # Simulate first round games
  semifinal_teams <- c()
  
  for (i in 1:length(bracket)) {
    game_name <- names(bracket)[i]
    teams <- bracket[[i]]
    
    winner <- simulate_game(teams[1], teams[2], team_ranks_df, verbose)
    semifinal_teams <- c(semifinal_teams, winner)
    
    if (verbose) {
      cat(sprintf("Quarterfinal %d: %s vs %s -> Winner: %s\n", 
                  i, teams[1], teams[2], winner))
    }
  }
  
  if (verbose) cat("\nSemifinals:\n")
  
  # Semifinals: winners of games 1&2, winners of games 3&4
  final_teams <- c()
  
  # Semifinal 1
  sf1_winner <- simulate_game(semifinal_teams[1], semifinal_teams[2], team_ranks_df, verbose)
  final_teams <- c(final_teams, sf1_winner)
  
  if (verbose) {
    cat(sprintf("Semifinal 1: %s vs %s -> Winner: %s\n", 
                semifinal_teams[1], semifinal_teams[2], sf1_winner))
  }
  
  # Semifinal 2  
  sf2_winner <- simulate_game(semifinal_teams[3], semifinal_teams[4], team_ranks_df, verbose)
  final_teams <- c(final_teams, sf2_winner)
  
  if (verbose) {
    cat(sprintf("Semifinal 2: %s vs %s -> Winner: %s\n", 
                semifinal_teams[3], semifinal_teams[4], sf2_winner))
  }
  
  # Championship
  if (verbose) cat("\n=== CHAMPIONSHIP GAME ===\n")
  
  champion <- simulate_game(final_teams[1], final_teams[2], team_ranks_df, verbose)
  runner_up <- ifelse(champion == final_teams[1], final_teams[2], final_teams[1])
  
  if (verbose) {
    cat(sprintf("\nCHAMPION: %s defeats %s\n", champion, runner_up))
  }
  
  return(list(
    champion = champion,
    runner_up = runner_up,
    finalists = final_teams,
    semifinalists = semifinal_teams
  ))
}

# Function to run multiple CWS simulations
run_cws_simulations <- function(team_ranks_df, bracket, n_sims = 1000, verbose = FALSE) {
  cat("Running", n_sims, "College World Series simulations...\n")
  
  results <- list()
  champions <- c()
  finalists <- c()
  semifinalists <- c()
  
  for (i in 1:n_sims) {
    if (i %% 100 == 0) cat("Completed", i, "simulations\n")
    
    sim_result <- simulate_cws_top8(team_ranks_df, bracket, verbose = FALSE)
    results[[i]] <- sim_result
    champions <- c(champions, sim_result$champion)
    finalists <- c(finalists, sim_result$finalists)
    semifinalists <- c(semifinalists, sim_result$semifinalists)
  }
  
  # Calculate probabilities
  champ_probs <- table(champions) / n_sims * 100
  champ_probs <- sort(champ_probs, decreasing = TRUE)
  
  finalist_probs <- table(finalists) / n_sims * 100
  finalist_probs <- sort(finalist_probs, decreasing = TRUE)
  
  semifinal_probs <- table(semifinalists) / n_sims * 100
  semifinal_probs <- sort(semifinal_probs, decreasing = TRUE)
  
  # Display results
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("COLLEGE WORLD SERIES SIMULATION RESULTS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cat("\nCHAMPIONSHIP PROBABILITIES:\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")
  for (i in 1:length(champ_probs)) {
    cat(sprintf("%s: %.1f%%\n", names(champ_probs)[i], champ_probs[i]))
  }
  
  cat("\nFINALS APPEARANCE PROBABILITIES:\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")
  for (i in 1:length(finalist_probs)) {
    cat(sprintf("%s: %.1f%%\n", names(finalist_probs)[i], finalist_probs[i]))
  }
  
  cat("\nSEMIFINALS APPEARANCE PROBABILITIES:\n")
  cat(paste(rep("-", 38), collapse = ""), "\n")
  for (i in 1:length(semifinal_probs)) {
    cat(sprintf("%s: %.1f%%\n", names(semifinal_probs)[i], semifinal_probs[i]))
  }
  
  return(list(
    results = results,
    championship_probabilities = champ_probs,
    finals_probabilities = finalist_probs,
    semifinals_probabilities = semifinal_probs,
    n_simulations = n_sims
  ))
}

# Function to analyze head-to-head matchups for each quarterfinal
analyze_quarterfinal_matchups <- function(team_ranks_df, bracket, n_sims = 1000) {
  cat("\nQUARTERFINAL MATCHUP ANALYSIS:\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  matchup_results <- list()
  
  for (i in 1:length(bracket)) {
    game_name <- names(bracket)[i]
    teams <- bracket[[i]]
    
    cat(sprintf("\nGame %d: %s vs %s\n", i, teams[1], teams[2]))
    cat(paste(rep("-", 25), collapse = ""), "\n")
    
    # Get Elo ratings
    elo1 <- team_ranks_df$`final.elos(elo_optim)`[team_ranks_df$school == teams[1]]
    elo2 <- team_ranks_df$`final.elos(elo_optim)`[team_ranks_df$school == teams[2]]
    
    cat(sprintf("%s Elo: %.0f\n", teams[1], elo1))
    cat(sprintf("%s Elo: %.0f\n", teams[2], elo2))
    
    # Calculate theoretical win probability
    win_prob1 <- elo.prob(elo1, elo2)
    
    cat(sprintf("%s win probability: %.1f%%\n", teams[1], win_prob1 * 100))
    cat(sprintf("%s win probability: %.1f%%\n", teams[2], (1 - win_prob1) * 100))
    
    # Simulate the matchup
    wins_team1 <- 0
    for (j in 1:n_sims) {
      winner <- simulate_game(teams[1], teams[2], team_ranks_df, verbose = FALSE)
      if (winner == teams[1]) wins_team1 <- wins_team1 + 1
    }
    
    simulated_prob1 <- wins_team1 / n_sims
    
    cat(sprintf("Simulated %s win rate: %.1f%%\n", teams[1], simulated_prob1 * 100))
    cat(sprintf("Simulated %s win rate: %.1f%%\n", teams[2], (1 - simulated_prob1) * 100))
    
    matchup_results[[game_name]] <- list(
      teams = teams,
      elo_ratings = c(elo1, elo2),
      theoretical_probs = c(win_prob1, 1 - win_prob1),
      simulated_probs = c(simulated_prob1, 1 - simulated_prob1)
    )
  }
  
  return(matchup_results)
}

quarterfinal_analysis <- analyze_quarterfinal_matchups(top8_team_ranks, cws_bracket, n_sims = 5000)

# Run full tournament simulation
cws_sim_results <- run_cws_simulations(top8_team_ranks, cws_bracket, n_sims = 5000)

# Create summary table for championship odds
championship_table <- data.frame(
  Team = names(cws_sim_results$championship_probabilities),
  Championship_Probability = as.numeric(cws_sim_results$championship_probabilities),
  Finals_Probability = as.numeric(cws_sim_results$finals_probabilities[names(cws_sim_results$championship_probabilities)]),
  Semifinals_Probability = as.numeric(cws_sim_results$semifinals_probabilities[names(cws_sim_results$championship_probabilities)])
) %>%
  mutate(
    Rank = row_number(),
    Championship_Pct = paste0(round(Championship_Probability, 1), "%"),
    Finals_Pct = paste0(round(Finals_Probability, 1), "%"),
    Semifinals_Pct = paste0(round(Semifinals_Probability, 1), "%")
  ) %>%
  select(Rank, Team, Championship_Pct, Finals_Pct, Semifinals_Pct)

cat("\nSUMMARY TABLE:\n")
print(championship_table)

# One simulation with detailed output
cat("\n\nSAMPLE TOURNAMENT SIMULATION:\n")
cat(paste(rep("=", 45), collapse = ""), "\n")
sample_result <- simulate_cws_top8(top8_team_ranks, cws_bracket, verbose = TRUE)


top8_viz_data <- top8_elo_data %>%
  mutate(
    championship_prob = as.numeric(cws_sim_results$championship_probabilities[team]),
    finals_prob = as.numeric(cws_sim_results$finals_probabilities[team]),
    semifinals_prob = as.numeric(cws_sim_results$semifinals_probabilities[team])
  ) %>%
  # Replace NAs with 0 for teams not in results
  mutate(
    championship_prob = ifelse(is.na(championship_prob), 0, championship_prob),
    finals_prob = ifelse(is.na(finals_prob), 0, finals_prob),
    semifinals_prob = ifelse(is.na(semifinals_prob), 0, semifinals_prob)
  ) %>%
  select(-semifinals_prob) %>%
  rename(c(`Elo Rating`= "elo_rating", 
           "Team"= "team", 
           `Top 64 Ranking` = "original_rank", 
           `Finals Appearance %` = "finals_prob",
           `CWS Champion %` = "championship_prob")) %>%
  select(Team, `Top 64 Ranking`, `Elo Rating`, `Finals Appearance %`, `CWS Champion %`)

team_schedules <- read.csv("team_schedules.csv")


# School colors for top 8 teams
top8_colors <- c(
  "Oregon St." = "#FF6600",         # Orange
  "Coastal Carolina" = "#006A4E",   # Teal
  "Arizona" = "#003366",            # Navy Blue
  "Louisville" = "#AD0000",         # Cardinal Red
  "Arkansas" = "#9D2235",           # Cardinal
  "UCLA" = "#2774AE",               # UCLA Blue
  "LSU" = "#461D7C",                # Purple
  "Murray St." = "#0033A0"          # Blue
)

# Filter data for top 8 teams
top8_2025_data <- team_schedules %>%
  mutate(Date = as.Date(Date)) %>%
  filter(team %in% top8_teams & year == 2025) %>%
  arrange(team, Date) %>%
  group_by(team) %>%
  mutate(game_number = row_number()) %>%
  ungroup()

# Create improved plot for top 8
top8_plot <- ggplot(top8_2025_data, aes(x = Date, y = team_elo_after, color = team)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 0.8, alpha = 0.7) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),  
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line()
  ) +
  labs(
    title = "NCAA Baseball CWS Teams - 2025 Elo Ratings",
    x = "Date",
    y = "Elo Rating", 
    color = "CWS Team"
  ) +
  scale_x_date(
    date_labels = "%b %d", 
    date_breaks = "2 weeks",
    expand = expansion(mult = c(0.02, 0.02))  
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.02)),  
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_color_manual(values = top8_colors) +
  geom_hline(yintercept = 1500, linetype = "dashed", alpha = 0.6, color = "gray50", size = 0.8) +
  coord_cartesian(clip = "off")

print(top8_plot)
