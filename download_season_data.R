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
  })
})

#order of data collection -> get all d1 team names -> using ncaa_teams function for years 2021-2025 
# get team ids using function ncaa_school_id_lu
# get all schedule info using ncaa_schedule_info function 
# clean up the schedule info

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

school_ids_by_year <- data.frame()
unique_d1_schools <- unique(d1_schools$team_name)
#get ids of each team for that year - shouldnt change y/y but still good to make sure
for (i in seq_along(unique_d1_schools)){
  u_school
}

for (u_school in unique_d1_schools){
  school_id <- ncaa_school_id_lu(team_name = u_school)
  school_ids_by_year <- rbind(school_ids_by_year,school_id)
  cat("Added", len(unique_d1_schools), "schools for", "\n")
}



ncaa_sched <- data.frame()
#loop through all school_ids and make a df
for (year in years){
  for (i in 1:nrow(d1_schools)){
    ncaa_info <- get_ncaa_schedule_info(d1_schools$school_id[i], year)
    print(d1_schools$school_id[i])
    print(d1_schools$school[i])
    ncaa_info$school <- d1_schools$school[i]
    ncaa_info$conference <- d1_schools$conference[i] 
    ncaa_sched <- rbind(ncaa_sched,ncaa_info) 
  }
}

length(unique(ncaa_sched$school))

d1_schools %>%
  filter(team_name == "Missouri" & year == 2025) 

ncaa_school_id_lu(team_name = "school")















