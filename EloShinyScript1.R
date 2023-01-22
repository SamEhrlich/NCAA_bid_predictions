#this script will be for loading in 2018-2022 data and merging the new data each week from the 2023 season

#begin by loading in packages and the csv for 2018-2022 that is already clean
suppressPackageStartupMessages({
  suppressWarnings({
    library(tidyverse)
    library(baseballr)
    library(elo)
    library(broom)
    library(lubridate)
    library(mgsub)
  })
})


clean_data <- read.csv('/Users/sam/Desktop/MizBaseball/Playoff Projection/CleanHalfSeason1822.csv')

#request all games that have been played in 2023 from baseballr using for loop, clean and bind data to schedule

#get the teams in the d1 division
d1_teams <- ncaa_team_lu %>%
  filter(division == 1, year == 2023)

ncaa_sched <- data.frame()

#loop through all school_ids and make a df
for (i in 1:nrow(d1_teams)){
  ncaa_info <- get_ncaa_schedule_info(d1_teams$school_id[i], 2023)
  print(d1_teams$school_id[i])
  print(d1_teams$school[i])
  ncaa_info$school <- d1_teams$school[i]
  ncaa_info$conference <- d1_teams$conference[i] 
  ncaa_sched <- rbind(ncaa_sched,ncaa_info) 
}

length(unique(ncaa_sched$school))

#do necessary cleaning to join this data to csv
#remove games that were canceled
ncaa_sched <- ncaa_sched %>% mutate_all(na_if,"")

#removes about 400 games
ncaa_sched <- ncaa_sched %>%
  drop_na(result)

#75 ties will remove for now could add back later
ncaa_sched <- ncaa_sched %>%
  filter(result != 'T')

#assign 0 for away games and 1 for home games using the @ in the opponent column
ncaa_sched$loc <- ifelse(grepl("@",ncaa_sched$opponent), 0, 1)
ncaa_sched$home_away <- case_when(
  grepl("@",ncaa_sched$opponent) ~ 'opponent_home',
  TRUE ~ 'school_home'
)

#remove the @ before each opponent and the whitespace following the @
ncaa_sched$opponent <- sub("^@+", "", ncaa_sched$opponent)

#trim whitespace before and after string
ncaa_sched$opponent <- trimws(ncaa_sched$opponent)

#grab neutral site from away games by finding strings that still contain an @
ncaa_sched$loc_neut <- ifelse(grepl('@', ncaa_sched$opponent),1,0)


#remove all @ after that indicates a neutral site
ncaa_sched$opponent <- gsub("@.*","",ncaa_sched$opponent)

#remove name of championship that starts with 2022 from opponent name after that indicates a neutral site
ncaa_sched$opponent <- gsub("2022.*","",ncaa_sched$opponent)
ncaa_sched$opponent <- gsub("2021.*","",ncaa_sched$opponent)

#remove rank from the opponent name by removing pound sign then number (probably could do both at the same time?)
ncaa_sched$opponent <- str_remove(ncaa_sched$opponent,"^#+")
ncaa_sched$opponent <- str_remove(ncaa_sched$opponent,"^[0-9]+")

#clean whitespace again
ncaa_sched$opponent <- trimws(ncaa_sched$opponent)

#change W/L to 1 and 0
ncaa_sched$result <- ifelse(ncaa_sched$result == 'L', 0, 1)

#drop unnecessary columns
ncaa_sched <- ncaa_sched %>%
  select(-c(X,innings,opponent_slug,slug))

ncaa_sched %>%
  group_by(year) %>%
  count()

#create a unique identifier for each game played by taking the id from within the box score url 
ncaa_sched$match_id <- as.numeric(gsub(".*?([0-9]+).*", "\\1", ncaa_sched$game_info_url)) 

#remove non conference games where the length of unique match ids are not 2
#408 non conference games
conference_matches <- ncaa_sched %>%
  count(match_id) %>%
  filter(n == 2 | n == 3) #conference games

#youngston state has 3 instances of the same game in 2018 the df need to keep them and sort to get youngston in 

#group by season and matchid to perform cleaning
half_new_season <- ncaa_sched %>%
  filter(year == 2023, match_id %in% conference_matches$match_id) %>%
  group_by(year, match_id) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  drop_na(game_info_url)

#split runs for school and opponent
half_new_season[c('school_runs', 'opponent_runs')] <- str_split_fixed(half_new_season$score, '-', 2)

#get the school conferences from team lookup
school_lu <- ncaa_team_lu %>%
  filter(year == 2023) %>%
  group_by(school) %>%
  dplyr::slice_tail(n = 1)


#a vector of schools and their conference they belong to
school_conf <- c(school_lu$conference)
names(school_conf) <- c(school_lu$school)
school_conf

half_new_season <- half_new_season %>%
  mutate(home_team = ifelse(home_away == 'school_home', school, opponent),
         away_team = ifelse(home_away == 'school_home', opponent, school),
         home_runs = ifelse(home_team == school, school_runs, opponent_runs),
         away_runs = ifelse(away_team == school, school_runs, opponent_runs),
         home_win = ifelse(home_runs > away_runs, 1,0),
         away_win = ifelse(home_win == 0, 1,0),
         home_conference = school_conf[home_team],
         away_conference = school_conf[away_team],
         home_conference = case_when(
           home_team == 'UAlbany' ~ 'America East',
           home_team == 'Nicholls' ~ 'Southland',
           home_team == 'Houston Christian' ~ 'Southland',
           home_team == 'NIU' ~ 'MAC',
           TRUE ~ home_conference
         ),
         away_conference = case_when(
           away_team == 'UAlbany' ~ 'America East',
           away_team == 'Nicholls' ~ 'Southland',
           away_team == 'Houston Christian' ~ 'Southland',
           away_team == 'NIU' ~ 'MAC',
           TRUE ~ away_conference
         )) %>%
  select(-c(school,opponent,result,school_runs, opponent_runs, game_info_url, conference, home_away, score))

#remove any non d1 schools
d2 <- c("SIAC","Gulf South","CCAA","GLVC","Lone Star","NE10","PacWest","RMAC","MIAC")

#remove d2 schools from game history
half_new_season <- half_new_season[ !grepl(paste(d2, collapse="|"), half_new_season$home_conference),]
half_new_season <- half_new_season[ !grepl(paste(d2, collapse="|"), half_new_season$away_conference),]

#split conferences into groups based on bid count
conference_group1 <- c('SEC','ACC') #best 2 conferences
conference_group2 <- c('Pac-12','Big 12') #2nd best 2 conferences
conference_group3 <- c('Big Ten','C-USA') #3rd best 2 conferences and then all other schools will not be grouped

#create groupings of conferences
half_new_season <- half_new_season %>%
  mutate(conference_group_1_home = ifelse(home_conference %in% conference_group1,1,0),
         conference_group_2_home = ifelse(home_conference %in% conference_group2,1,0),
         conference_group_3_home = ifelse(home_conference %in% conference_group3,1,0),
         conference_group_1_away = ifelse(away_conference %in% conference_group1,1,0),
         conference_group_2_away = ifelse(away_conference %in% conference_group2,1,0),
         conference_group_3_away = ifelse(away_conference %in% conference_group3,1,0))

#create last year bid and rolling winpct columns
year_2022_bids <- c('Ole Miss','Oklahoma','Arkansas','Texas A&M','Auburn','Notre Dame',
                    'Stanford','Texas','East Carolina','Louisville','North Carolina',
                    'Oregon St.','Southern Miss.','Tennessee','UConn','Virginia Tech',
                    'Air Force','Arizona','Coastal Carolina','Columbia','Florida',
                    'Georgia Tech','LSU','Maryland','Michigan','Oklahoma St.','TCU',
                    'Texas St.','Texas Tech','UCLA','Vanderbilt','VCU','Campbell',
                    'Central Mich.','Florida St.','Georgia','Ga. Southern','Gonzaga',
                    'Kennesaw St.','Louisiana','Louisiana Tech','Miami (FL)','Missouri St.',
                    'Oregon','San Diego','UC Santa Barbara','Virginia','Wake Forest',
                    'Alabama St.','Army West Point','Binghamton','Canisius',
                    'Coppin St.','DBU','Grand Canyon','Hofstra','Liberty','LIU',
                    'New Mexico St.','Oral Roberts','Southeast Mo. St.','Southeastern La.',
                    'UNC Greensboro','Wright St.')

#create last year bid column
half_new_season <- half_new_season %>%
  mutate(last_year_bid_home = 0,
         last_year_bid_home = ifelse(home_team %in% year_2022_bids & year == 2023, 1,last_year_bid_home),
         last_year_bid_away = 0,
         last_year_bid_away = ifelse(away_team %in% year_2022_bids & year == 2023, 1,last_year_bid_away))

#now the data is cleaned and ready for joining to the entire schedule barring any new changes in 2023 schedule that is different from previous years

all_seasons_clean <- rbind(clean_data,half_new_season)

#just make sure the new columns being bound are not repeats and save new csv

write.csv(df_name, 'path to save.csv', rownames = FALSE)

#could run elo model here or within shiny app? 
#making changes based on user input 56 times?




