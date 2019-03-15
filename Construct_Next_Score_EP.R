# Title: Construct_Next_Score_EP.R
# Author: Aaron Weisberg
# Created: Jan 29, 2017
# Last Edit: Feb 04, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and figures out the next scoring
#   play.  That data is then attached to the play by 
#   play data to allow construction of an expected
#   points (EP) model for the NFL.

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

# Set up points added function
# Points should be based on type of scoring play
# Account for goofy data anomaly where 1 play is listed a a TD and safety, should be a safety
calculate_points_added <- function(points.data, data.out){
  #Calculate Points Added by Every Scoring Play
  data.out <- data.frame(points.data)
  data.out <- data.out %>% mutate(Points.Scored = ifelse(Touchdown == 1 & (InterceptionThrown == 1 || (Fumble == 1 & RecFumbTeam == DefensiveTeam)), 0-6,
                                    ifelse(Touchdown == 1 & Safety == 0, 6, 
                                            ifelse(grepl("^Good$", FieldGoalResult, ignore.case = T), 3,
                                                   ifelse(grepl("Made", ExPointResult, ignore.case = T), 1,
                                                        ifelse(grepl("Success", TwoPointConv, ignore.case = T), 2,
                                                            ifelse(Safety == 1, 0-2, 
                                                                   ifelse(grepl("Success", DefTwoPoint, ignore.case = T), 0-2, NA))))))))                                                  
  return(data.out)
}

# Set up function to find next score for every game in data set
# Things to keep in mind
# 1 - The next score in the same half should be the points added
# 2 - If the next score is by an opponent, the number should be negative
# 3 - Extra points/2 point conversions come after touchdowns and should only have their own value
#     so they should be treated separately
# 4 - Defensive scores are negative points added
find_next_score <- function(data.in){
    data.in <- data.in[order(data.in$GameID, -data.in$TimeSecs),]
    data.in$Play.ID <- as.numeric(rownames(data.in))
    end.half <- data.in %>% group_by(GameID, Half) %>% arrange(-TimeSecs) %>% slice(n()) %>% ungroup
    data.in$Points.Scored[(data.in$Play.ID %in% end.half$Play.ID) & is.na(data.in$Points.Scored)] <- 0
    data.in <- data.in %>% mutate(Score.Team = ifelse(!is.na(Points.Scored) & Points.Scored >= 0, posteam,
                                                        ifelse(!is.na(Points.Scored) & Points.Scored < 0, DefensiveTeam, NA)))
    data.in <- data.in %>% group_by(GameID) %>% mutate(Points.Added = na.locf(Points.Scored, na.rm = F , fromLast = T)) %>% ungroup
    data.in <- data.in %>% group_by(GameID) %>% mutate(Scoring.Team = na.locf(Score.Team, na.rm = F, fromLast = T)) %>% ungroup
    data.in <- data.in %>% mutate(Points.Added = ifelse(Scoring.Team == posteam, 
                                                         abs(Points.Added), -1 * abs(Points.Added)))
    data.out <- data.frame(data.in)
  return(data.out)
}

# Set up log file
log_filename <- paste(path_log, "Construct_Next_Score_EP_",
                      gsub(":", "_", gsub("-", "_", gsub(" ", "_", Sys.Date()))), ".txt", sep="")
sink(file = log_filename, append = F, type = c("output", "message"), split = T)

# Load Play by Play data
pbp_data_filename <- paste(path_data, "All_Play_By_Play_Data_2009_2016.rdata", sep="")
load(file = pbp_data_filename)

# Fix TimeSecs since there were negative values in it when it was scraped
mutate(master_data, TimeSecs = 3600 - (as.numeric(substr(time, 1, 2)) * 60 * as.numeric(qtr)) + as.numeric(substr(time, 4, 5)))

# Subset to only scoring plays - test points scored function
touchdowns <- master_data[master_data$Touchdown == 1, ]
safety <- master_data[master_data$Safety == 1 , ]
DefTwoPoint <- master_data[master_data$DefTwoPoint == "Success" & !(is.na(master_data$DefTwoPoint)), ]
FieldGoalMade <- master_data[master_data$FieldGoalResult == "Good" & !(is.na(master_data$FieldGoalResult)), ]
PATGood <- master_data[master_data$ExPointResult == "Made" & !(is.na(master_data$ExPointResult)), ]
# 
scoring_plays <- rbind(touchdowns, safety, DefTwoPoint, FieldGoalMade, PATGood)

scoring_plays <- scoring_plays[order(scoring_plays$GameID, scoring_plays$Drive),]
# Calculate actual points added by each scoring play
scoring_with_points <- calculate_points_added(scoring_plays, scoring_with_points)

# Ok it looks like it worked, lets see if we can make some logic to create a half ID
# Make Half variable - what half is it, 3 means OT
scoring_with_points$Half <- as.factor(ifelse(as.numeric(scoring_with_points$qtr) <= 2, 1, 
                                  ifelse(as.numeric(scoring_with_points$qtr) <= 4, 2, 3)))

# Ok we know the points scored function and half logic works, lets apply it to the master data 
master_data <- calculate_points_added(master_data, master_data)
master_data$Half <- as.factor(ifelse(as.numeric(master_data$qtr) <= 2, 1, 
                                             ifelse(as.numeric(master_data$qtr) <= 4, 2, 3)))

# Apply find_next_score function to master data - get points added for every play
master_data_points_added <- find_next_score(master_data)

# Save master data with points added
filename <- paste(path_data, "2009_2016_Points_Added_Next_Score_Same_Half_Method.rdata", sep="")
save(master_data_points_added, file = filename)

# Close Log
sink()