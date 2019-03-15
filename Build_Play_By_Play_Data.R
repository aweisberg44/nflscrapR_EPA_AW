# Title: Build_Play_By_Play_Data.R
# Author: Aaron Weisberg
# Created: January 27, 2017
# Last Edit: Feb 04, 2017
# Description: Build play by play data set with
#   every play in the NFL from 2009 to 2016

# Load Required Packages
require(nflscrapR)
require(dplyr)
require(tidyr)
require(plyr)
require(ggplot2)
require(utils)

rm(list=ls())

# Set paths for log and data export
path_log <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_export <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"

# Set Up log File
log_file <- paste(path_log, "PBP_Master_Data_Create_", gsub(" ", "_", gsub(":", " ", gsub("-", ":", Sys.time()))), ".txt", sep = "")
sink(file = log_file, append = F, type = c("output", "message"), split = T)

# Load all game level data sets and keep unique gameID field
years <- 2009:2016

for(i in 1:length(years)){
  yr <- years[i]
  filename <- paste(path_export, "NFL_Game_Data_", yr, ".csv", sep="")
  var_name <- paste("season_", yr, sep="")
  assign(var_name, as.data.frame(read.csv(filename)))

}

# Stack seasons together
season_2009$Season <- 2009
season_2010$Season <- 2010
season_2011$Season <- 2011
season_2012$Season <- 2012
season_2013$Season <- 2013
season_2014$Season <- 2014
season_2015$Season <- 2015
season_2016$Season <- 2016

all_seasons <- rbind(season_2009, season_2010,
                     season_2011, season_2012, season_2013,
                     season_2014, season_2015, season_2016)

# Get array of all game IDs
all_game_ids <- all_seasons[, c(2, 8)]

# Check for duplicates
anyDuplicated(all_game_ids)

# Ok we have no duplicated game IDs which is good
# Lets iterate through game IDs to get a complete play by play data set
# Step 1 - Set blank data frame with 68 columns 
master_data <- NULL

# Step 2 - Load NFL game by game data and stack into single master data frame
for(i in 1:length(all_game_ids$GameID)){
  game_id <- all_game_ids$GameID[i]
  next_game_data <- game_play_by_play(game_id)
  next_game_data$Season <- all_game_ids$Season[i]
  master_data <- rbind(next_game_data, master_data)
}

# Step 3 - export final data frame for season long and play by play data
filename <- paste(path_export, "All_Play_By_Play_Data_2009_2016.rdata", sep="")
save(master_data, file = filename)
filename <- paste(path_export, "All_Play_By_Play_Data_2009_2016.csv", sep="")
write.csv(master_data, file = filename)

filename <- paste(path_export, "All_Game_Data_2009_2016.rdata", sep="")
save(all_seasons, file = filename)
filename <- paste(path_export, "All_Game_Data_2009_2016.csv", sep="")
write.csv(all_seasons, file = filename)

# End Log File
sink()