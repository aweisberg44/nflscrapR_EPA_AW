# Title: Variable_Exploration_EP_Neutral2.R
# Author: Aaron Weisberg
# Created: Feb 13, 2017
# Last Edit: Feb 13, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and explores the EP training data for relationhips
#   between points added and key scoring drivers in football using
#   principal component analysis 


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
log_filename <- paste(path_log, "Variable_Exploration_EP_Neutral2_",
                      gsub(":", "_", gsub("-", "_", gsub(" ", "_", Sys.Date()))), ".txt", sep="")
sink(file = log_filename, append = F, type = c("output", "message"), split = T)

# Bring in training and validation data sets
filename <- paste(path_data, "trn_data_post_deep_dive.rdata", sep="")
load(filename)
filename <- paste(path_data, "val_data_post_deep_dive.rdata", sep="")
load(filename)

#  Construct Principal Component Analysis on key pre-snap fields
#  Check names and subset fields
names(trn_data)
pca.fields <- c("Drive"   ,           "qtr"          ,      "down"               ,"time"          ,     "TimeUnder"     ,    
                "TimeSecs" ,          "PlayTimeDiff"  ,     "SideofField"        ,"yrdln"          ,    "yrdline100"   ,      "ydstogo" ,                       
                "GoalToGo"  ,         "FirstDown"      ,    "posteam"            ,"DefensiveTeam"   ,       
                "sp"         ,        "Touchdown"       ,   "ExPointResult"      ,"TwoPointConv"    ,  "Onsidekick"  ,     
                "PuntResult"  ,       "PlayType"         ,  "FieldGoalDistance" ,
                "Challenge.Replay",   "ChalReplayResult"  ,   
                "PosTeamScore"     ,  "DefTeamScore"       ,"ScoreDiff"        ,  "AbsScoreDiff"    ,           
                "Season"             ,"Half"            ,          
                "PAT_Attempt",        "Challenge.Occurred" ,"Play.Overturned",   
                "Play.Upheld")
pca.data <- trn_data[ , names(trn_data) %in% pca.fields]
#PCA Requires Numeric, so lets check that really quick
sapply(pca.data, function(x) is.numeric(x))
#Ok there are a few we can fix easy like qtr, down, and Half and then we can do our PCA for the numeric stuff
#We also know those fields are important from the forest, so maybe we just remove them and see what else comes up
num.fields <- sapply(pca.data, is.numeric)
pca.data <- pca.data[, num.fields]
# Make sure we only keep fields that have a standard deviation
stdevs <- data.frame(sapply(pca.data, sd))
colnames(stdevs) <- "Standard.Deviation"
stdevs$variables <- rownames(stdevs)
no.stdev <- stdevs[stdevs$Standard.Deviation == 0 | is.na(stdevs$Standard.Deviation), 'variables']
length(no.stdev)
# Ok everyone has a standard deviation. Lets Go
formula1 <- as.formula(~.)
pca1 <- prcomp(formula = formula1, data=pca.data, center = T, scale = T)
# Lets see which components really matter
pngfile <- paste(path_out, "Principal_Component_Analysis_", gsub("-", "_", Sys.Date()), ".png", sep="")
png(filename = pngfile)
plot(pca1, type = "l")
dev.off()
# Alright looks like componenets 1-3 are the biggest, 4-5 matter a little bit, and after 5 they really tail off
# Lets take a look at the numbers
print(pca1)
# Lets look at proportion of variance
summary(pca1)
# Ok it really is only 1-4 that matter at all. Lets take a closer look at those in the log file and see what we can gather
# Looks like lots of the same fields we got in the forest.  I am pretty confident about which fields we need at this point
# More importantly, the modeling methods will likely change the results
# Perhaps the best approach is an ensemble model of several methods with similar sets of variables
# Close Log
sink()