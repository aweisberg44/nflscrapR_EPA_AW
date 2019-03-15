# Title: Build_Drive_By_Drive_Data.R
# Author: Aaron Weisberg
# Created: January 28, 2017
# Last Edit: January 28, 2017
# Description: Build drive by drive data set with
#   every drive in the NFL from 2009 to 2016


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
path_data <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"

# Set Up log File
log_file <- paste(path_log, "DBD_Master_Data_Create_", gsub(" ", "_", gsub(":", " ", gsub("-", ":", Sys.time()))), ".txt", sep = "")
sink(file = log_file, append = F, type = c("output", "message"), split = T)

# Load master list of all game data
filename <- paste(path_data, "All_Game_Data_2009_2016.rdata", sep="")
load(file = filename)

all_game_ids <- all_seasons[, 2]
master_data <- data.frame(matrix(vector(),0, 16))

# Load NFL drive by drive data and stack into single master data frame
for(i in 1:length(all_game_ids)){
  game_id <- all_game_ids[i]
  next_game_data <- drive_summary(game_id)
  master_data <- rbind(next_game_data, master_data)
}

filename <- paste(path_data, "All_Drive_Data_2009_2016.rdata", sep="")
save(master_data, file = filename)
filename <- paste(path_data, "All_Drive_Data_2009_2016.csv", sep="")
write.csv(master_data, file = filename)

# End Log File
sink()
