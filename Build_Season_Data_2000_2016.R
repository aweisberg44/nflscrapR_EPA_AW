# Title: Build_Season_Data_2000_2016.R
# Author: Aaron Weisberg              
# Created: January 26, 2017           
# Last edit: January 26, 2017         
# Description:  This code will create data frames with game information
#   for every NFL game from 2009 through 2016.  The data frames will then
#   be saved on my laptop until a server or drive is added to store them
#   permanently.



# Load Required Packages
require(nflscrapR)
require(dplyr)
require(tidyr)
require(plyr)
require(ggplot2)
require(utils)

# Set paths for log and data export
path_log <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_export <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"

# Set Up log File
log_file <- paste(path_log, "Season_Data_Export_", gsub(" ", "_", gsub(":", " ", gsub("-", ":", Sys.time()))), ".txt", sep = "")
sink(file = log_file, append = F, type = c("output", "message"), split = T)

# Create Loop to load and store data
for (year in 2009:2016){
  yr <- as.numeric(year)
  filename <- paste(path_export, "NFL_Game_Data_", yr, ".rdata", sep="")
  filename_csv = gsub(".rdata", ".csv",filename)
  data_file <- season_games(yr)
  save(data_file, file = filename, compress = T)
  write.csv(data_file, file = filename_csv)
}
  
# End Log
sink()