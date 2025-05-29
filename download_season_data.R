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
write.csv(school_ids_by_year, "d1_schools2125ids_raw.csv", row.names = FALSE)

#join ids to schools 
d1_school_info <- d1_schools %>%
  select(team_name,conference_id,division,year) %>%
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
  filter(division == 1 & team_name == 'Missouri')

d1_school_info %>%
  filter(division == 1 & conference_id  == '827')

school_ids_by_year

#### 2025 teams team_id and conference 


ncaa_schedule_info(team_id = 434, year = 2025)

######## NEEED SEASONID TO SCRAPE THE SEASON USING BASEBALLR. MAYBE LOOP THROUGH ALL TEAMS 2025 SEASONS AND GET THE SEASON ID FROM THE URL OR SOMETHING IDK TONGITH WE GRIND



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










