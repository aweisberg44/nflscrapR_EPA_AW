# Title: Data_Deep_Dive_EP.R
# Author: Aaron Weisberg
# Created: Feb 04, 2017
# Last Edit: Feb 09, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and explores the EP training data for relationhips
#   between points added and key scoring drivers in football.
#   Examples of such fields include field position, number
#   of timeouts, down and distance, scoring differential,
#   and time on the clock.  There is some data cleaning
#   performed at the end of the script to allow for 
#   further data exploration later.

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

# Set up file paths
rm(list=ls())
path_data <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"
path_log  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_out  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Analysis\\"

# Set up log file
log_filename <- paste(path_log, "Data_Deep_Dive_",
                      gsub(":", "_", gsub("-", "_", gsub(" ", "_", Sys.Date()))), ".txt", sep="")
sink(file = log_filename, append = F, type = c("output", "message"), split = T)


# Bring in training and validation data sets
filename <- paste(path_data, "EPA_Next_Score_Method_Trn.rdata", sep="")
load(filename)
filename <- paste(path_data, "EPA_Next_Score_Method_Val.rdata", sep="")
load(filename)

print("Names of training data fields")
names(trn_data)
print("Names of validation data fields")
names(val_data)

print("Classes of training data fields")
sapply(trn_data, class)
print("Classes of validation data fields")
sapply(val_data, class)

# Create data frame with names and classes of every field
data.info <- NULL
data.info <- data.frame(sapply(trn_data, class))
colnames(data.info) <- "Class"
data.info$Variable <- rownames(data.info)

filename <- paste(path_data, "EP_trn_field_class_info.rdata", sep="")
save(data.info, file = filename, compress = T)
filename <- gsub(".rdata", ".csv", filename)
write.csv(data.info, file = filename)

# Ok after a closer look, we can get rid of the existing WPA and EPA fields
# Since we are going to build our own model, we do not need theirs until later for benchmarking
# But for development, we can get rid of them
# Everything else is a valid predictor for now
WP.Vars <- grep("WP", data.info$Variable, value = T)
ExpPts.Vars <- grep("ExpPts", data.info$Variable, value = T)
trn_data <- trn_data[ , !(names(trn_data) %in% WP.Vars) & !(names(trn_data) %in% ExpPts.Vars)]
val_data <- val_data[ , !(names(val_data) %in% WP.Vars) & !(names(val_data) %in% ExpPts.Vars)]


# Ok now that we have slightly trimmed down our data set, lets start searching for predictors of points added
# Lets make a correlation table. Do not care about covariance since the scaling is often way off for that
# Subset to only numeric useful fields
numeric.vars <- sapply(trn_data, is.numeric)
Not.Necessary.Vars <- c("Play.ID", "Points.Scored", "PlayAttempted")
trn_numeric <- trn_data[ , numeric.vars & !(names(trn_data) %in% Not.Necessary.Vars)]
pearson.table <- data.frame(cor(trn_numeric, method = "pearson", use ='na.or.complete'))
spearman.table <- data.frame(cor(trn_numeric, method = "spearman", use ='na.or.complete'))

filename <- paste(path_out, "Pearson_Correlation_Matrix", gsub("-", "_", Sys.Date()), ".rdata", sep="")
save(pearson.table, file = filename)
gsub(".rdata", ".csv", filename)
write.csv(pearson.table, file = filename)

filename <- paste(path_out, "Spearman_Correlation_Matrix", gsub("-", "_", Sys.Date()), ".rdata", sep="")
save(spearman.table, file = filename)
gsub(".rdata", ".csv", filename)
write.csv(pearson.table, file = filename)

pdf.filename <- paste(path_out, "Pearson_Correlation_Matrix", gsub("-", "_", Sys.Date()), ".pdf", sep="")
pdf(file = pdf.filename)
corrplot(cor(trn_numeric, method = "pearson", use ='na.or.complete'), method = "circle", na.label = "NA", na.label.col = "purple")
dev.off()
pdf.filename <- paste(path_out, "Spearman_Correlation_Matrix", gsub("-", "_", Sys.Date()), ".pdf", sep="")
pdf(file = pdf.filename)
corrplot(cor(trn_numeric, method = "spearman", use ='na.or.complete'), method = "circle", na.label = "NA", na.label.col = "purple")
dev.off()


# Check key character fields and factors for average points added by level
trn_non_numeric <- trn_data[ , (!(names(trn_data) %in% names(trn_numeric)) & !(names(trn_data) %in% Not.Necessary.Vars)) | names(trn_data) %in% "Points.Added"]
names(trn_non_numeric)
Not.Neccessary.Vars2 <- c("GameID", "Date", "Tackler1", "Tackler2", "Partition", "BlockingPlayer", "desc")
trn_non_numeric <- trn_non_numeric[ , !(names(trn_non_numeric) %in% Not.Neccessary.Vars2)]
names(trn_non_numeric)

# Ok lets look at some of our time related variables:
# qtr and Half are 2 obvious ones
trn_non_numeric <- data.table(trn_non_numeric)
qtr.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "qtr"]
Half.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "Half"]

title <- "Points Scored by Quarter"
qtr.plot <- ggplot(data = qtr.data, aes(x = qtr, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("Quarter")
qtr.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = qtr.plot)

title <- "Points Scored by Half"
Half.plot <- ggplot(data = Half.data, aes(x = Half, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("Half")
Half.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = Half.plot)

# Ok now lets check some down, field position, and distance variables
# down, SideofField
down.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "down"]
SideofField.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "SideofField"]

title <- "Points Scored by Down"
down.plot <- ggplot(data = down.data, aes(x = down, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("Down")
down.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = down.plot)

title <- "Points Scored by Side of Field"
SideofField.plot <- ggplot(data = SideofField.data, aes(x = SideofField, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("SideofField")
SideofField.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = SideofField.plot)

# Ok lets take a quick peak at some play attributes
# PlayType, PassLocation, RunLocation, RunGap, PenaltyType
PlayType.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "PlayType"]
PassLocation.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "PassLocation"]

title <- "Points Scored by Play Type"
PlayType.plot <- ggplot(data = PlayType.data, aes(x = PlayType, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("Play Type")
PlayType.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = PlayType.plot)

title <- "Points Scored by Pass Location"
PassLocation.plot <- ggplot(data = PassLocation.data, aes(x = PassLocation, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("PassLocation")
PassLocation.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = PassLocation.plot)

RunLocation.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "RunLocation"]
RunGap.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "RunGap"]

title <- "Points Scored by Run Location"
RunLocation.plot <- ggplot(data = RunLocation.data, aes(x = RunLocation, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("Run Location")
RunLocation.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = RunLocation.plot)

title <- "Points Scored by Run Gap"
RunGap.plot <- ggplot(data = RunGap.data, aes(x = RunGap, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("Run Gap")
RunGap.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = RunGap.plot)


PenaltyType.data <- trn_non_numeric[ , list(avg.points.added = mean(Points.Added)), by = "PenaltyType"]

title <- "Points Scored by Penalty Type"
PenaltyType.plot <- ggplot(data = PenaltyType.data, aes(x = PenaltyType, y = avg.points.added)) + ggtitle(title) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Average Points Added") + xlab("PenaltyType")
PenaltyType.plot
filename <- paste(path_out, gsub(" ", "_", title), "_", gsub("-", "_", Sys.Date()), ".png", sep = "")
ggsave(filename = filename, plot = PenaltyType.plot)

########################### CLEAN DATA FOR POTENTIAL RANDOM FOREST EVALUATION #############################
# Ok this was pretty helpful. At least now we know that on average these fields make a difference
# We also learned there is still some data cleaning to be done
# Lets clean up a few fields, then maybe we can try a random forest or CHAID tree to help us with some variable selection
# First data cleaning item - fix all of the NAs for things like extra points, field goals, ect with binary indicator variables
sum(is.na(trn_data$ExPointResult))
sum(is.na(trn_data$TwoPointConv))
sum(is.na(trn_data$DefTwoPoint))

trn_data <- trn_data %>% mutate(PAT_Attempt = ifelse(!(is.na(ExPointResult)) | !(is.na(TwoPointConv)) | !(is.na(DefTwoPoint)),
                                                     1, 0))
sum(is.na(trn_data$PAT_Attempt))
trn_data <- data.table(trn_data)
trn_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("ExPointResult")]
trn_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("TwoPointConv")]
trn_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("DefTwoPoint")]
trn_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("DefTwoPoint", "TwoPointConv", "ExPointResult")]
# Ok looks like our PAT_Attempt flag is constructed properly
# Lets fill in some binary indicators now
trn_data <- trn_data %>% mutate(ExPointMade = ifelse(ExPointResult == "Made" & !(is.na(ExPointResult)), 1, 0))
trn_data <- trn_data %>% mutate(TwoPointMade = ifelse(TwoPointConv == "Success" & !(is.na(TwoPointConv)), 1, 0))
trn_data <- trn_data %>% mutate(DefTwoPointMade = ifelse(DefTwoPoint == "Success" & !(is.na(DefTwoPoint)), 1, 0))
trn_data[ , list(SUM_ExPointMade = sum(ExPointMade)), by = c("ExPointResult")]
trn_data[ , list(SUM_TwoPointMade = sum(TwoPointMade)), by = c("TwoPointConv")]
trn_data[ , list(SUM_DefTwoPointMade = sum(DefTwoPointMade)), by = c("DefTwoPoint")]
# Those flags look like they are properly constructed
# Lets Fix Play attempted
sum(is.na(trn_data$PlayAttempted))
# Nevermind it doesn't need to be fixed
# Lets check rush attempt and pass attempt
sum(is.na(trn_data$RushAttempt))
sum(is.na(trn_data$PassAttempt))
# Those look good as well
# Lets check our challenge related fields
sum(is.na(trn_data$Challenge.Replay))
sum(is.na(trn_data$ChalReplayResult))
trn_data[ , list(Count = sum(PlayAttempted)), by = c("ChalReplayResult")]
trn_data <- trn_data %>% mutate(Challenge.Occurred = ifelse(!(is.na(ChalReplayResult)), 1, 0),
                                Play.Overturned = ifelse(ChalReplayResult == 'Reversed' & !(is.na(ChalReplayResult)), 1, 0),
                                Play.Upheld = ifelse(ChalReplayResult == 'Upheld' & !(is.na(ChalReplayResult)), 1, 0))
# Those should help us figure out challenge related items
# Lets check out our fields from the kicking game and see if we can spruce some of those up with indicator variables as well
sum(is.na(trn_data$Onsidekick))
sum(is.na(trn_data$PuntResult))
sum(is.na(trn_data$FieldGoalResult))
sum(is.na(trn_data$FieldGoalDistance))
# Ok onside kick is fine, but the other 3 need to be taken care of before we can do exploratory variable searching since
#   kicking plays may have a unique effect on points added
trn_data <- trn_data %>% mutate(PuntBlocked = ifelse(PuntResult == 'Blocked' & !(is.na(PuntResult)), 1, 0),
                                FieldGoalMade = ifelse(FieldGoalResult == 'Good' & !(is.na(FieldGoalResult)), 1, 0),
                                FieldGoalBlocked = ifelse(FieldGoalResult == 'Blocked' & !(is.na(FieldGoalResult)), 1, 0),
                                KickBlocked = ifelse((PuntResult == 'Blocked' | FieldGoalResult == 'Blocked') 
                                                     & !(is.na(PuntResult)) & !(is.na(FieldGoalResult)), 1, 0))
trn_data$FieldGoalDistance <- as.numeric(trn_data$FieldGoalDistance)
trn_data$FieldGoalDistance[is.na(trn_data$FieldGoalDistance)] <- 0
sum(is.na(trn_data$FieldGoalDistance))
# Ok those look better now
# Lets take a look at making some return game variables to clean up missing values there as well
sum(is.na(trn_data$ReturnResult))
trn_data <- trn_data %>% mutate(ReturnTD = ifelse(ReturnResult == 'Touchdown' & !(is.na(ReturnResult)), 1, 0),
                                FairCatch = ifelse(ReturnResult == 'Fair Catch' & !(is.na(ReturnResult)), 1, 0),
                                ReturnTouchback = ifelse(ReturnResult == "Touchback" & !(is.na(ReturnResult)), 1, 0))
# Ok now lets check some passing game related fields like completions, deep pass, and short pass to make sure
#   we clean up any potential missing values there as well
sum(is.na(trn_data$PassOutcome))
sum(is.na(trn_data$PassLength))
trn_data <- trn_data %>% mutate(CompletedPass = ifelse(PassOutcome == "Complete" & !(is.na(PassOutcome)), 1, 0),
                                DeepPass = ifelse(PassLength == "Deep" & !(is.na(PassLength)), 1, 0))

############################### Add these new fields to validation data as well ############################################
# Ok this was pretty helpful. At least now we know that on average these fields make a difference
# We also learned there is still some data cleaning to be done
# Lets clean up a few fields, then maybe we can try a random forest or CHAID tree to help us with some variable selection
# First data cleaning item - fix all of the NAs for things like extra points, field goals, ect with binary indicator variables
sum(is.na(val_data$ExPointResult))
sum(is.na(val_data$TwoPointConv))
sum(is.na(val_data$DefTwoPoint))

val_data <- val_data %>% mutate(PAT_Attempt = ifelse(!(is.na(ExPointResult)) | !(is.na(TwoPointConv)) | !(is.na(DefTwoPoint)),
                                                     1, 0))
sum(is.na(val_data$PAT_Attempt))
val_data <- data.table(val_data)
val_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("ExPointResult")]
val_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("TwoPointConv")]
val_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("DefTwoPoint")]
val_data[ , list(SUM_PAT_ATTEMPT = sum(PAT_Attempt)), by = c("DefTwoPoint", "TwoPointConv", "ExPointResult")]
# Ok looks like our PAT_Attempt flag is constructed properly
# Lets fill in some binary indicators now
val_data <- val_data %>% mutate(ExPointMade = ifelse(ExPointResult == "Made" & !(is.na(ExPointResult)), 1, 0))
val_data <- val_data %>% mutate(TwoPointMade = ifelse(TwoPointConv == "Success" & !(is.na(TwoPointConv)), 1, 0))
val_data <- val_data %>% mutate(DefTwoPointMade = ifelse(DefTwoPoint == "Success" & !(is.na(DefTwoPoint)), 1, 0))
val_data[ , list(SUM_ExPointMade = sum(ExPointMade)), by = c("ExPointResult")]
val_data[ , list(SUM_TwoPointMade = sum(TwoPointMade)), by = c("TwoPointConv")]
val_data[ , list(SUM_DefTwoPointMade = sum(DefTwoPointMade)), by = c("DefTwoPoint")]
# Those flags look like they are properly constructed
# Lets Fix Play attempted
sum(is.na(val_data$PlayAttempted))
# Nevermind it doesn't need to be fixed
# Lets check rush attempt and pass attempt
sum(is.na(val_data$RushAttempt))
sum(is.na(val_data$PassAttempt))
# Those look good as well
# Lets check our challenge related fields
sum(is.na(val_data$Challenge.Replay))
sum(is.na(val_data$ChalReplayResult))
val_data[ , list(Count = sum(PlayAttempted)), by = c("ChalReplayResult")]
val_data <- val_data %>% mutate(Challenge.Occurred = ifelse(!(is.na(ChalReplayResult)), 1, 0),
                                Play.Overturned = ifelse(ChalReplayResult == 'Reversed' & !(is.na(ChalReplayResult)), 1, 0),
                                Play.Upheld = ifelse(ChalReplayResult == 'Upheld' & !(is.na(ChalReplayResult)), 1, 0))
# Those should help us figure out challenge related items
# Lets check out our fields from the kicking game and see if we can spruce some of those up with indicator variables as well
sum(is.na(val_data$Onsidekick))
sum(is.na(val_data$PuntResult))
sum(is.na(val_data$FieldGoalResult))
sum(is.na(val_data$FieldGoalDistance))
# Ok onside kick is fine, but the other 3 need to be taken care of before we can do exploratory variable searching since
#   kicking plays may have a unique effect on points added
val_data <- val_data %>% mutate(PuntBlocked = ifelse(PuntResult == 'Blocked' & !(is.na(PuntResult)), 1, 0),
                                FieldGoalMade = ifelse(FieldGoalResult == 'Good' & !(is.na(FieldGoalResult)), 1, 0),
                                FieldGoalBlocked = ifelse(FieldGoalResult == 'Blocked' & !(is.na(FieldGoalResult)), 1, 0),
                                KickBlocked = ifelse(PuntResult == 'Blocked' | FieldGoalResult == 'Blocked'
                                                     & !(is.na(FieldGoalResult)) & !(is.na(PuntResult)), 1, 0))
val_data$FieldGoalDistance <- as.numeric(val_data$FieldGoalDistance)
val_data$FieldGoalDistance[is.na(val_data$FieldGoalDistance)] <- 0
sum(is.na(val_data$FieldGoalDistance))
# Ok those look better now
# Lets take a look at making some return game variables to clean up missing values there as well
sum(is.na(val_data$ReturnResult))
val_data <- val_data %>% mutate(ReturnTD = ifelse(ReturnResult == 'Touchdown' & !(is.na(ReturnResult)), 1, 0),
                                FairCatch = ifelse(ReturnResult == 'Fair Catch' & !(is.na(ReturnResult)), 1, 0),
                                ReturnTouchback = ifelse(ReturnResult == "Touchback" & !(is.na(ReturnResult)), 1, 0))
# Ok now lets check some passing game related fields like completions, deep pass, and short pass to make sure
#   we clean up any potential missing values there as well
sum(is.na(val_data$PassOutcome))
sum(is.na(val_data$PassLength))
val_data <- val_data %>% mutate(CompletedPass = ifelse(PassOutcome == "Complete" & !(is.na(PassOutcome)), 1, 0),
                                DeepPass = ifelse(PassLength == "Deep" & !(is.na(PassLength)), 1, 0))

# Ok all the indicators we wanted are set up.  Let's do some work on cleaning up the factor data
names(Filter(is.factor, trn_data))
# Ok none of those should be missing, but lets double check
sum(is.na(trn_data$qtr))
sum(is.na(trn_data$down))
sum(is.na(trn_data$Half))

# Fix Down
trn_data$down <- as.character(trn_data$down)
trn_data$down[is.na(trn_data$down)] <- "No Down"
sum(is.na(trn_data$down))

# Lets check all of our character fields. Some of these we made indicators for, but we should fill the NAs in
#   with something for use in random forests
names(Filter(is.character, trn_data))
# alright lets double check for some obvious ones we know shouldn't have an NAs
sum(is.na(trn_data$GameID))
sum(is.na(trn_data$time))
sum(is.na(trn_data$SideofField))
sum(is.na(trn_data$posteam))
sum(is.na(trn_data$DefensiveTeam))
sum(is.na(trn_data$desc))
sum(is.na(trn_data$HomeTeam))
sum(is.na(trn_data$AwayTeam))
sum(is.na(trn_data$Scoring.Team))
sum(is.na(trn_data$Partition))

# Turns out defensive team and side of field need some light work
trn_data$SideofField[is.na(trn_data$SideofField)] <- "Missing"
trn_data$DefensiveTeam[is.na(trn_data$DefensiveTeam)] <- "Missing"



# Some of these we don't need for expected points modeling - get rid of them
trn_data$Passer <- NULL
trn_data$Interceptor <- NULL
trn_data$Rusher <- NULL
trn_data$Receiver <- NULL
trn_data$Returner <- NULL
trn_data$BlockingPlayer <- NULL
trn_data$Tackler1 <- NULL
trn_data$Tackler2 <- NULL
trn_data$RecFumbPlayer <- NULL
trn_data$PenalizedPlayer <- NULL

# For the rest of these - fill in with applicable values
# Check missing first
sum(is.na(trn_data$ExPointResult))
sum(is.na(trn_data$TwoPointConv))
sum(is.na(trn_data$DefTwoPoint))
sum(is.na(trn_data$PuntResult))
sum(is.na(trn_data$PlayType))
sum(is.na(trn_data$PassOutcome))
sum(is.na(trn_data$PassLength))
sum(is.na(trn_data$PassLocation))
sum(is.na(trn_data$RunLocation))
sum(is.na(trn_data$RunGap))
sum(is.na(trn_data$ReturnResult))
sum(is.na(trn_data$FieldGoalResult))
sum(is.na(trn_data$RecFumbTeam))
sum(is.na(trn_data$ChalReplayResult))
sum(is.na(trn_data$PenalizedTeam))
sum(is.na(trn_data$PenaltyType))

# Besides PlayType, all of these have missing values to fill in
trn_data$ExPointResult[is.na(trn_data$ExPointResult)] <- "No Attempt"
table(trn_data$ExPointResult)
trn_data$TwoPointConv[is.na(trn_data$TwoPointConv)] <- "No Attempt"
table(trn_data$TwoPointConv)
trn_data$DefTwoPoint[is.na(trn_data$DefTwoPoint)] <- "No Attempt"
table(trn_data$DefTwoPoint)

trn_data$PuntResult[is.na(trn_data$PuntResult)] <- "No Punt"
table(trn_data$PuntResult)
trn_data$PassOutcome[is.na(trn_data$PassOutcome)] <- "No Pass"
table(trn_data$PassOutcome)
trn_data$PassLength[is.na(trn_data$PassLength)] <- "No Pass"
table(trn_data$PassLength)

trn_data$PassLocation[is.na(trn_data$PassLocation)] <- "No Pass"
table(trn_data$PassLocation)
trn_data$RunLocation[is.na(trn_data$RunLocation)] <- "No Rush"
table(trn_data$RunLocation)
trn_data$RunGap[is.na(trn_data$RunGap)] <- "No Rush"
table(trn_data$RunGap)

trn_data$ReturnResult[is.na(trn_data$ReturnResult)] <- "No Return"
table(trn_data$ReturnResult)
trn_data$FieldGoalResult[is.na(trn_data$FieldGoalResult)] <- "No FG"
table(trn_data$FieldGoalResult)
trn_data$RecFumbTeam[is.na(trn_data$RecFumbTeam)] <- "No Fumble"
table(trn_data$RecFumbTeam)

trn_data$ChalReplayResult[is.na(trn_data$ChalReplayResult)] <- "No Challenge"
table(trn_data$ChalReplayResult)
trn_data$PenalizedTeam[is.na(trn_data$PenalizedTeam)] <- "No Penalty"
table(trn_data$PenalizedTeam)
trn_data$PenaltyType[is.na(trn_data$PenaltyType)] <- "No Penalty"
table(trn_data$PenaltyType)
# All good, but penalty type is a bit long.  Lets get rid of it since it isn't really suitable for modeling
# If we need it later, we can bring it back from the master data file
trn_data$PenaltyType <- NULL

# We should also check the numeric fields for NAs. Most of these should be fine, but lets double check
names(Filter(is.numeric, trn_data))
# Ok first lets check the ones we know are fine
sum(is.na(trn_data$Drive))
sum(is.na(trn_data$TimeUnder))
sum(is.na(trn_data$TimeSecs))
sum(is.na(trn_data$PlayTimeDiff))
sum(is.na(trn_data$yrdln))
sum(is.na(trn_data$yrdline100))
sum(is.na(trn_data$ydstogo))
sum(is.na(trn_data$ydsnet))
sum(is.na(trn_data$GoalToGo))
sum(is.na(trn_data$FirstDown))
sum(is.na(trn_data$PlayAttempted))
sum(is.na(trn_data$Yards.Gained))

sum(is.na(trn_data$sp))
sum(is.na(trn_data$Touchdown))
sum(is.na(trn_data$Safety))
sum(is.na(trn_data$Onsidekick))
sum(is.na(trn_data$PassAttempt))
sum(is.na(trn_data$InterceptionThrown))

sum(is.na(trn_data$RushAttempt))
sum(is.na(trn_data$Reception))
sum(is.na(trn_data$Fumble))
sum(is.na(trn_data$Sack))
sum(is.na(trn_data$Challenge.Replay))
sum(is.na(trn_data$Accepted.Penalty))

sum(is.na(trn_data$Penalty.Yards))
sum(is.na(trn_data$PosTeamScore))
sum(is.na(trn_data$DefTeamScore))
sum(is.na(trn_data$ScoreDiff))
sum(is.na(trn_data$AbsScoreDiff))
sum(is.na(trn_data$Accepted.Penalty))
sum(is.na(trn_data$Play.ID))
sum(is.na(trn_data$Points.Added))
sum(is.na(trn_data$Season))

# Ok we have to fix TimeSecs, PlayTimeDiff, yrdln, yrdline100, GoalToGo,
#   FirstDown, PlayAttempted, PosTeamScore, DefTeamScore,ScoreDiff, AbsScoreDiff
# Lets get the easy ones we can set = 0 first
trn_data$PlayTimeDiff[is.na(trn_data$PlayTimeDiff)] <- 0
trn_data$PlayAttempted[is.na(trn_data$PlayAttempted)] <- 0
trn_data$FirstDown[is.na(trn_data$FirstDown)] <- 0
trn_data$GoalToGo[is.na(trn_data$GoalToGo)] <- 0

# OK for these just take the value from the last completed play
trn_data <- trn_data %>% group_by(GameID) %>% mutate(TimeSecs = na.locf(TimeSecs, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(trn_data$TimeSecs))
trn_data <- trn_data %>% group_by(GameID) %>% mutate(yrdln = na.locf(yrdln, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(trn_data$yrdln))
trn_data <- trn_data %>% group_by(GameID) %>% mutate(yrdline100 = na.locf(yrdline100, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(trn_data$yrdline100))
trn_data <- trn_data %>% group_by(GameID) %>% mutate(PosTeamScore = na.locf(PosTeamScore, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(trn_data$PosTeamScore))
trn_data <- trn_data %>% group_by(GameID) %>% mutate(DefTeamScore = na.locf(DefTeamScore, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(trn_data$DefTeamScore))
trn_data <- trn_data %>% group_by(GameID) %>% mutate(ScoreDiff = na.locf(ScoreDiff, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(trn_data$ScoreDiff))
trn_data <- trn_data %>% group_by(GameID) %>% mutate(AbsScoreDiff = na.locf(AbsScoreDiff, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(trn_data$AbsScoreDiff))

# Ok it's all clean. Now lets apply it to the validation data set
# Ok all the indicators we wanted are set up.  Let's do some work on cleaning up the factor data
names(Filter(is.factor, val_data))
# Ok none of those should be missing, but lets double check
sum(is.na(val_data$qtr))
sum(is.na(val_data$down))
sum(is.na(val_data$Half))

# Fix down for kicking situations and extra points
val_data$down <- as.character(val_data$down)
val_data$down[is.na(val_data$down)] <- "No Down"
sum(is.na(val_data$down))

# Lets check all of our character fields. Some of these we made indicators for, but we should fill the NAs in
#   with something for use in random forests
names(Filter(is.character, val_data))
# alright lets double check for some obvious ones we know shouldn't have an NAs
sum(is.na(val_data$GameID))
sum(is.na(val_data$time))
sum(is.na(val_data$SideofField))
sum(is.na(val_data$posteam))
sum(is.na(val_data$DefensiveTeam))
sum(is.na(val_data$desc))
sum(is.na(val_data$HomeTeam))
sum(is.na(val_data$AwayTeam))
sum(is.na(val_data$Scoring.Team))
sum(is.na(val_data$Partition))

# Some of these we don't need for expected points modeling - get rid of them
val_data$Passer <- NULL
val_data$Interceptor <- NULL
val_data$Rusher <- NULL
val_data$Receiver <- NULL
val_data$Returner <- NULL
val_data$BlockingPlayer <- NULL
val_data$Tackler1 <- NULL
val_data$Tackler2 <- NULL
val_data$RecFumbPlayer <- NULL
val_data$PenalizedPlayer <- NULL

# For the rest of these - fill in with applicable values
# Check missing first
sum(is.na(val_data$ExPointResult))
sum(is.na(val_data$TwoPointConv))
sum(is.na(val_data$DefTwoPoint))
sum(is.na(val_data$PuntResult))
sum(is.na(val_data$PlayType))
sum(is.na(val_data$PassOutcome))
sum(is.na(val_data$PassLength))
sum(is.na(val_data$PassLocation))
sum(is.na(val_data$RunLocation))
sum(is.na(val_data$RunGap))
sum(is.na(val_data$ReturnResult))
sum(is.na(val_data$FieldGoalResult))
sum(is.na(val_data$RecFumbTeam))
sum(is.na(val_data$ChalReplayResult))
sum(is.na(val_data$PenalizedTeam))
sum(is.na(val_data$PenaltyType))

# Besides PlayType, all of these have missing values to fill in
val_data$ExPointResult[is.na(val_data$ExPointResult)] <- "No Attempt"
table(val_data$ExPointResult)
val_data$TwoPointConv[is.na(val_data$TwoPointConv)] <- "No Attempt"
table(val_data$TwoPointConv)
val_data$DefTwoPoint[is.na(val_data$DefTwoPoint)] <- "No Attempt"
table(val_data$DefTwoPoint)

val_data$PuntResult[is.na(val_data$PuntResult)] <- "No Punt"
table(val_data$PuntResult)
val_data$PassOutcome[is.na(val_data$PassOutcome)] <- "No Pass"
table(val_data$PassOutcome)
val_data$PassLength[is.na(val_data$PassLength)] <- "No Pass"
table(val_data$PassLength)

val_data$PassLocation[is.na(val_data$PassLocation)] <- "No Pass"
table(val_data$PassLocation)
val_data$RunLocation[is.na(val_data$RunLocation)] <- "No Rush"
table(val_data$RunLocation)
val_data$RunGap[is.na(val_data$RunGap)] <- "No Rush"
table(val_data$RunGap)

val_data$ReturnResult[is.na(val_data$ReturnResult)] <- "No Return"
table(val_data$ReturnResult)
val_data$FieldGoalResult[is.na(val_data$FieldGoalResult)] <- "No FG"
table(val_data$FieldGoalResult)
val_data$RecFumbTeam[is.na(val_data$RecFumbTeam)] <- "No Fumble"
table(val_data$RecFumbTeam)

val_data$ChalReplayResult[is.na(val_data$ChalReplayResult)] <- "No Challenge"
table(val_data$ChalReplayResult)
val_data$PenalizedTeam[is.na(val_data$PenalizedTeam)] <- "No Penalty"
table(val_data$PenalizedTeam)
val_data$PenaltyType[is.na(val_data$PenaltyType)] <- "No Penalty"
table(val_data$PenaltyType)
# All good, but penalty type is a bit long.  Lets get rid of it since it isn't really suitable for modeling
# If we need it later, we can bring it back from the master data file
val_data$PenaltyType <- NULL

# We should also check the numeric fields for NAs. Most of these should be fine, but lets double check
names(Filter(is.numeric, val_data))
# Ok first lets check the ones we know are fine
sum(is.na(val_data$Drive))
sum(is.na(val_data$TimeUnder))
sum(is.na(val_data$TimeSecs))
sum(is.na(val_data$PlayTimeDiff))
sum(is.na(val_data$yrdln))
sum(is.na(val_data$yrdline100))
sum(is.na(val_data$ydstogo))
sum(is.na(val_data$ydsnet))
sum(is.na(val_data$GoalToGo))
sum(is.na(val_data$FirstDown))
sum(is.na(val_data$PlayAttempted))
sum(is.na(val_data$Yards.Gained))

sum(is.na(val_data$sp))
sum(is.na(val_data$Touchdown))
sum(is.na(val_data$Safety))
sum(is.na(val_data$Onsidekick))
sum(is.na(val_data$PassAttempt))
sum(is.na(val_data$InterceptionThrown))

sum(is.na(val_data$RushAttempt))
sum(is.na(val_data$Reception))
sum(is.na(val_data$Fumble))
sum(is.na(val_data$Sack))
sum(is.na(val_data$Challenge.Replay))
sum(is.na(val_data$Accepted.Penalty))

sum(is.na(val_data$Penalty.Yards))
sum(is.na(val_data$PosTeamScore))
sum(is.na(val_data$DefTeamScore))
sum(is.na(val_data$ScoreDiff))
sum(is.na(val_data$AbsScoreDiff))
sum(is.na(val_data$Accepted.Penalty))
sum(is.na(val_data$Play.ID))
sum(is.na(val_data$Points.Added))
sum(is.na(val_data$Season))

# Ok we have to fix TimeSecs, PlayTimeDiff, yrdln, yrdline100, GoalToGo,
#   FirstDown, PlayAttempted, PosTeamScore, DefTeamScore,ScoreDiff, AbsScoreDiff
# Lets get the easy ones we can set = 0 first
val_data$PlayTimeDiff[is.na(val_data$PlayTimeDiff)] <- 0
val_data$PlayAttempted[is.na(val_data$PlayAttempted)] <- 0
val_data$FirstDown[is.na(val_data$FirstDown)] <- 0
val_data$GoalToGo[is.na(val_data$GoalToGo)] <- 0

# OK for these just take the value from the last completed play
val_data <- val_data %>% group_by(GameID) %>% mutate(TimeSecs = na.locf(TimeSecs, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(val_data$TimeSecs))
val_data <- val_data %>% group_by(GameID) %>% mutate(yrdln = na.locf(yrdln, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(val_data$yrdln))
val_data <- val_data %>% group_by(GameID) %>% mutate(yrdline100 = na.locf(yrdline100, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(val_data$yrdline100))
val_data <- val_data %>% group_by(GameID) %>% mutate(PosTeamScore = na.locf(PosTeamScore, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(val_data$PosTeamScore))
val_data <- val_data %>% group_by(GameID) %>% mutate(DefTeamScore = na.locf(DefTeamScore, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(val_data$DefTeamScore))
val_data <- val_data %>% group_by(GameID) %>% mutate(ScoreDiff = na.locf(ScoreDiff, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(val_data$ScoreDiff))
val_data <- val_data %>% group_by(GameID) %>% mutate(AbsScoreDiff = na.locf(AbsScoreDiff, na.rm = F, fromLast = F)) %>% ungroup
sum(is.na(val_data$AbsScoreDiff))

# Save data sets for future use
filename <- paste(path_data, "trn_data_post_deep_dive.rdata", sep="")
save(trn_data, file = filename)
filename <- gsub(".rdata", ".csv", filename)
write.csv(trn_data, file = filename)

filename <- paste(path_data, "val_data_post_deep_dive.rdata", sep="")
save(val_data, file = filename)
filename <- gsub(".rdata", ".csv", filename)
write.csv(val_data, file = filename)

# Close Log
sink()
