# Title: Variable_Exploration_EP_Neutral.R
# Author: Aaron Weisberg
# Created: Feb 09, 2017
# Last Edit: Feb 12, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and explores the EP training data for relationhips
#   between points added and key scoring drivers in football using
#   random forest 

# Load required packages
require(utils)
require(dplyr)
require(tidyr)
require(plyr)
require(ggplot2)
require(scales)
require(zoo)
require(corrplot)
require(rpart)
require(rpart.plot)
require(data.table)
require(stats)
require(randomForest)

# Set file paths and clear data
rm(list=ls())
path_data <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"
path_log  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_out  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Analysis\\"

# Set up Log File
log_filename <- paste(path_log, "Variable_Exploration_EP_Neutral_",
                      gsub(":", "_", gsub("-", "_", gsub(" ", "_", Sys.Date()))), ".txt", sep="")
sink(file = log_filename, append = F, type = c("output", "message"), split = T)

# Bring in training and validation data sets
filename <- paste(path_data, "trn_data_post_deep_dive.rdata", sep="")
load(filename)
filename <- paste(path_data, "val_data_post_deep_dive.rdata", sep="")
load(filename)

# Check names
names(trn_data)
names(val_data)

# ok lets get rid of desc since doesn't do anything for us
trn_data$desc <- NULL
val_data$desc <- NULL

# Alright lets use a random forest to hep us figure out which of this stuff is important
trn_data <- data.frame(trn_data)
# get rid of ID variables and non-pre-snap variables
do.not.model <- c("GameID", "Date", "Play.ID", "Score.Team", "Points.Scored", "Score.Team", "Partition", "Points.Added",
                  "time", "Scoring.Team", "DefensiveTeam", "AwayTeam", "HomeTeam", "Touchdown", "FieldGoalResult", "ReturnResult",
                  "PuntResult", "InterceptionThrown", "RecFumbTeam", "PenalizedTeam", "Reception", "PassOutcome", "CompletedPass",
                  "DeepPass", "KickBlocked", "ReturnTD", "FairCatch", "ReturnTouchback", "FieldGoalMade", "FieldGoalBlocked",
                  "posteam", "ydsnet", "Yards.Gained", "Fumble", "PassLocation", "ExPointResult", "Accepted.Penalty", "Penalty.Yards",
                  "PassLength", "ExPointMade", "Safety", "ChalReplayResult", "sp", "TwoPointMade", "Sack", "FirstDown")
forestdata <- trn_data[ ,!(names(trn_data) %in% do.not.model)]
# Double check for no missing values
sapply(forestdata, function(x) sum(is.na(x)))
# Turn character to factors
forestdata[sapply(forestdata, is.character)] <- lapply(forestdata[sapply(forestdata, is.character)], 
                                       as.factor)
# On to the forest
formula1 <- as.formula(Points.Added~.)
forest1 <- randomForest::randomForest(x = forestdata, y = trn_data$Points.Added, ntree = 500,
                                      nodesize = 100, importance = T, na.action = na.omit)
png.file <- paste(path_out, "Variable_Importance_Plot_First_Forest_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.file)
varImpPlot(forest1, main = "forest1 Variable Importance - Pre-Snap decisions only")
dev.off()

# Ok some stuff we learned
# confirmed intuition regarding field position, down, distance, and time left on clock
# time between plays and score differential may be important
# Play selection pre-snap may be important - problem: this is hard to incorporate in a prediction model when we don't know
#   the plays in advance.  We may have to think about this one
# PATs will have to be modeled separately. They clearly differentiate points added since you can get a max of 2 points on PATs
# Coach's challenege and replay related variables are important.  Perhaps they have something to do with momentum in addition to
#   their influence on field position, down, distance, points, etc.  Again for live scoring this may prove difficult as we don't
#   know if a play is challenged until after it ends.  We could try to transform/lag it to use if the previous play was challenged
#   or overturned in some way.  That way we already have an outcome for these challenges and we test it's influence on the rest of 
#   the play or drive

# Lets see what happens if we do a forest on only plays that are not PATs

forestdata2 <- forestdata[forestdata$PAT_Attempt == 0, -28]
new.response <- as.data.frame(trn_data[trn_data$PAT_Attempt == 0, 61])
colnames(new.response) <- "Points.Added"
forest2 <- randomForest::randomForest(x = forestdata2, y = new.response$Points.Added, ntree = 500,
                                      nodesize = 100, importance = T, na.action = na.omit)
png.file <- paste(path_out, "Variable_Importance_Plot_Remove_PATs", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.file)
varImpPlot(forest2, main = "forest2 Variable Importance - Pre-Snap decisions only - No PATs")
dev.off()

# At this point I think we can be pretty confident that a model using yrdline100, down, distance, score diff, and
#     timesecs should be fairly predictive.  At this point, lets move on to actually building some models with the
#     most important variables.
# Close Log
sink()