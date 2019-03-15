# Title: Sample_NFL_2009_2016.R
# Author: Aaron Weisberg
# Created: Feb 04, 2017
# Last Edit: Feb 04, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and samples the data into training,
#   validation, and scoring data sets

# Load required packages
require(utils)
require(dplyr)
require(tidyr)
require(plyr)
require(ggplot2)
require(scales)
require(zoo)

# Set up file paths
rm(list=ls())
path_data <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"
path_log  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_out  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Analysis\\"

# Set up log file
log_filename <- paste(path_log, "Data_Deep_Dive_",
                      gsub(":", "_", gsub("-", "_", gsub(" ", "_", Sys.Date()))), ".txt", sep="")
sink(file = log_filename, append = F, type = c("output", "message"), split = T)

# Load master_data_with_points_scored
filename <- paste(path_data, '2009_2016_Points_Added_Next_Score_Same_Half_Method.rdata', sep="")
load(file = filename)

# Sanity Check - Should have 256 games per season since we have no playoff games
games_season <- master_data_points_added[ , c("GameID", "Season")]
unique_games <- as.data.frame(unique(games_season))
nrow(unique_games)
print("We should have 256 games every regular season")
unique_games %>% group_by(Season) %>% summarise(game.count = n())

# Remove 2016 season - use as scoring data set after models are built
scoring_hldout_data <- master_data_points_added[master_data_points_added$Season == 2016,]
trn_val_data <- master_data_points_added[!(master_data_points_added$Season == 2016),]

# Select unique game IDs from 2009 - 2015 and split 60-40 into training and validation
unique_game_ids_trn_val <- as.data.frame(unique(trn_val_data$GameID))
colnames(unique_game_ids_trn_val) <- "GameID"

# Set up desired sample size for training
sample.values <- c("T", "V")

# Set seed
seed <- 12345
set.seed(seed)

# Sample unique game IDs from 2009-2015
unique_game_ids_trn_val$Partition <- sample(sample.values, replace = T, nrow(unique_game_ids_trn_val), prob=c(0.6, 0.4))

#Check sampling worked at GameID Level
table(unique_game_ids_trn_val$Partition)
# T    V 
# 1024  768

print("Check Sampling Method - GameID Level")
print(1024/(1024+768))
print(768/(1024+768))

#Apply game level sampling to play by play level data
nrow(trn_val_data)
nrow(merge(x = trn_val_data, y = unique_game_ids_trn_val, by = "GameID", all.x = T))
trn_val_data <- merge(x = trn_val_data, y = unique_game_ids_trn_val, by = "GameID", all.x = T)

#Check sampling worked at Play Level
table(trn_val_data$Partition)
# T      V 
# 180968 135743 

print("Check Sampling Method - Play Level")
print(180968/(180968+135743))
print(135743/(180968+135743))

# Ok the split isn't perfect, but it is good enough for use in my opinion.
# This is especially true since 2016 is a true holdout
# Let's officially split.  We can check that the training and validation
#   samples have similar univariate statistics after the data is cleaned
trn_data <- trn_val_data[trn_val_data$Partition == 'T',] 
val_data <- trn_val_data[trn_val_data$Partition == 'V',]

# Let's save our datasets
filename <- paste(path_data, "EPA_Next_Score_Method_Trn.rdata", sep="")
save(trn_data, file = filename, compress = T)
filename <- paste(path_data, "EPA_Next_Score_Method_Val.rdata", sep="")
save(val_data, file = filename, compress = T)
filename <- paste(path_data, "EPA_Next_Score_Method_Trn_Val.rdata", sep="")
save(trn_val_data, file = filename, compress = T)
filename <- paste(path_data, "EPA_Next_Score_Method_Master.rdata", sep="")
save(master_data_points_added, file = filename, compress = T)


# Close Log
sink()
