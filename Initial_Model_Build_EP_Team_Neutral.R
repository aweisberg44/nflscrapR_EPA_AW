# Title: Initial_Model_Build_EP_Team_Neutral.R
# Author: Aaron Weisberg
# Created: Feb 12, 2017
# Last Edit: Feb 19, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and explores the EP training data for relationhips
#   between points added and key scoring drivers in football using
#   simple OLS regression

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
require(ROCR)
require(utils)
require(caret)

# Set file paths and clear data
rm(list=ls())
path_data <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"
path_log  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_out  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Analysis\\"

# Set up Log File
log_filename <- paste(path_log, "Initial_Model_Build_EP_Team_Neutral_",
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

# divide data into PATs and non-PATs - learning from trees and PCA says this makes a difference
# theoreticall they should be modeled separately anyway. Max points on PAT is 2, max otherwise is 6
# PAT also only occurs after TDs and you can choose where ball is placed (side of field wise) which makes them special
trn_data_TD <- trn_data[trn_data$PlayType != 'Extra Point', ]
trn_data_PAT <- trn_data[trn_data$PlayType == 'Extra Point', ]
# Double check it worked
table(trn_data_TD$PAT_Attempt)
table(trn_data_PAT$PAT_Attempt)
table(trn_data$PAT_Attempt)
table(trn_data$ExPointResult)
table(trn_data_PAT$ExPointResult)
table(trn_data_TD$ExPointResult)
# Uh ok. we have TD data that says it's PATs and PAT data that says it isn't a PAT.
# Check play descriptions
qc.pat <- trn_data_PAT[trn_data_PAT$PAT_Attempt == 0,]
# Ok those are actually PATs despite their mislabel. We are ok there
qc.td <- trn_data_TD[trn_data_TD$PAT_Attempt == 1, ]
# these all appear to be 2 point converstions
# note - clean up some of these PAT related variables before use in win probability modeling later
table(trn_data$ExPointResult)
table(trn_data$TwoPointConv)
table(trn_data$DefTwoPoint)
trn_data_PAT <- trn_data[trn_data$ExPointResult != 'No Attempt' | trn_data$TwoPointConv != 'No Attempt' | trn_data$DefTwoPoint != 'No Attempt', ]
table(trn_data_PAT$ExPointResult)
table(trn_data_PAT$TwoPointConv)
table(trn_data_PAT$DefTwoPoint)
trn_data_PAT <- data.table(trn_data_PAT)
trn_data_PAT[trn_data_PAT$ExPointResult == 'No Attempt' & trn_data_PAT$TwoPointConv == 'No Attempt' & trn_data_PAT$DefTwoPoint == 'No Attempt',
             list(Cnt = sum(PlayAttempted))]
# Ok good
trn_data_PAT <- data.frame(trn_data_PAT)
# set up TD data
trn_data_TD <- trn_data[trn_data$ExPointResult == 'No Attempt' & trn_data$TwoPointConv == 'No Attempt' & trn_data$DefTwoPoint == 'No Attempt', ]
trn_data_TD <- data.table(trn_data_TD)
trn_data_TD[trn_data_TD$ExPointResult != 'No Attempt' | trn_data_TD$TwoPointConv != 'No Attempt' |
                             trn_data_TD$DefTwoPoint != 'No Attempt', list(Cnt = sum(PlayAttempted))]
# One last QC check
nrow(trn_data_TD)
nrow(trn_data_PAT)
nrow(trn_data)
nrow(trn_data_TD) + nrow(trn_data_PAT)

# Ok lets do this for validation data as well
val_data_PAT <- val_data[val_data$ExPointResult != 'No Attempt' | val_data$TwoPointConv != 'No Attempt' | val_data$DefTwoPoint != 'No Attempt', ]
val_data_TD <- val_data[val_data$ExPointResult == 'No Attempt' & val_data$TwoPointConv == 'No Attempt' & val_data$DefTwoPoint == 'No Attempt', ]

# Set first formula with key variables based on forest, correlation, PCA, and general knowledge
formula1 <- as.formula(Points.Added ~ yrdline100 + TimeSecs + down + ydstogo + AbsScoreDiff + GoalToGo)
trn_data_TD$down <- as.factor(trn_data_TD$down)
trn_data_TD$down <- relevel(trn_data_TD$down, ref = 1)

# Basic linear regression
reg1_EP_TN <- lm(formula = formula1, data = trn_data_TD)
reg1_EP_TN
anova(reg1_EP_TN)
summary(reg1_EP_TN)
# AbsScoreDiff not significant. Get rid of it
formula2 <- as.formula(Points.Added ~ yrdline100 + TimeSecs + down + ydstogo + GoalToGo)
reg2_EP_TN <- lm(formula = formula2, data = trn_data_TD)
reg2_EP_TN
anova(reg2_EP_TN)
summary(reg2_EP_TN)

# Ok lets check out the predicted values
trn_data_TD$pred.reg <- predict(reg2_EP_TN, newdata = trn_data_TD, type = 'response')
val_data_TD$pred.reg <- predict(reg2_EP_TN, newdata = val_data_TD, type = 'response')

postResample(pred = trn_data_TD$pred.reg, obs = trn_data_TD$Points.Added)
postResample(pred = val_data_TD$pred.reg, obs = val_data_TD$Points.Added)

# Sanity Check - check predicted vs actual values visually
trn_data_TD <- data.table(trn_data_TD)
val_data_TD <- data.table(val_data_TD)

pdf.filename <- paste(path_out, "Basic_Regression_Visualized_Performance_", gsub(":", "_", Sys.Date()), ".pdf", sep = "")
pdf(file = pdf.filename)
trn_TD_down <- trn_data_TD[ ,list(Predicted = mean(pred.reg), Actual = mean(Points.Added)),
                        by = c("down", "yrdline100")]

trn_TD_down2 <- melt.data.table(trn_TD_down, id.vars = c("down", "yrdline100"),
                                measure.vars = c("Predicted", "Actual"), value.name = "Points_Added")
title <- "Predicted vs Actual Points Scored by Field Position and Down - Training Data"
down.plot <- ggplot(data = trn_TD_down2, aes(x=yrdline100, y = Points_Added, colour = variable, group = variable)) + ggtitle(title) +
  geom_line(aes(size = .2)) + geom_point(shape = 15) + facet_grid(down~.) + scale_size(range = c(.1, 1.5), guide = F) + scale_shape_discrete(solid = T)
print(down.plot)
# No down looks odd, others look good

trn_TD_gtg <- trn_data_TD[ ,list(Predicted = mean(pred.reg), Actual = mean(Points.Added)),
                            by = c("down", "GoalToGo")]

trn_TD_gtg2 <- melt.data.table(trn_TD_gtg, id.vars = c("down", "GoalToGo"),
                                measure.vars = c("Predicted", "Actual"), value.name = "Points_Added")
title <- "Predicted vs Actual Points Scored by Down and Goal To Go - Training Data"
gtg.plot <- ggplot(data = trn_TD_gtg2, aes(x=GoalToGo, y = Points_Added, fill = variable, group = variable)) + ggtitle(title) +
  geom_bar(position = position_dodge(), stat = 'identity') + facet_grid(down~.) + coord_flip()
print(gtg.plot)
# No down looks odd, others look good

trn_TD_time <- trn_data_TD[ ,list(Predicted = mean(pred.reg), Actual = mean(Points.Added)),
                            by = c("TimeUnder", "qtr")]

trn_TD_time2 <- melt.data.table(trn_TD_time, id.vars = c("TimeUnder", "qtr"),
                                measure.vars = c("Predicted", "Actual"), value.name = "Points_Added")
title <- "Predicted vs Actual Points Scored by Time Remaining in Quarter - Training Data"
time.plot <- ggplot(data = trn_TD_time2, aes(x = TimeUnder, y = Points_Added, colour = variable, group = variable)) + ggtitle(title) + 
  geom_line(aes(size = .2)) + geom_point(shape = 15) + facet_grid(qtr~.) + scale_size(range = c(.1, 1.5), guide = F) + scale_shape_discrete(solid = T)
print(time.plot)
# Looks good except for last 2 minutes of quarters. might need some bucketing on this one

# Lets check the same visuals on the validation data
val_TD_down <- val_data_TD[ ,list(Predicted = mean(pred.reg), Actual = mean(Points.Added)),
                            by = c("down", "yrdline100")]

val_TD_down2 <- melt.data.table(val_TD_down, id.vars = c("down", "yrdline100"),
                                measure.vars = c("Predicted", "Actual"), value.name = "Points_Added")
title <- "Predicted vs Actual Points Scored by Field Position and Down - Validation Data"
down.plot <- ggplot(data = val_TD_down2, aes(x=yrdline100, y = Points_Added, colour = variable, group = variable)) + ggtitle(title) +
  geom_line(aes(size = .2)) + geom_point(shape = 15) + facet_grid(down~.) + scale_size(range = c(.1, 1.5), guide = F) + scale_shape_discrete(solid = T)
print(down.plot)
# No down looks odd, others look good

val_TD_gtg <- val_data_TD[ ,list(Predicted = mean(pred.reg), Actual = mean(Points.Added)),
                           by = c("down", "GoalToGo")]

val_TD_gtg2 <- melt.data.table(val_TD_gtg, id.vars = c("down", "GoalToGo"),
                               measure.vars = c("Predicted", "Actual"), value.name = "Points_Added")
title <- "Predicted vs Actual Points Scored by Down and Goal To Go - Validation Data"
gtg.plot <- ggplot(data = val_TD_gtg2, aes(x=GoalToGo, y = Points_Added, fill = variable, group = variable)) + ggtitle(title) +
  geom_bar(position = position_dodge(), stat = 'identity') + facet_grid(down~.) + coord_flip()
print(gtg.plot)
# No down looks odd, others look good

val_TD_time <- val_data_TD[ ,list(Predicted = mean(pred.reg), Actual = mean(Points.Added)),
                            by = c("TimeUnder", "qtr")]

val_TD_time2 <- melt.data.table(val_TD_time, id.vars = c("TimeUnder", "qtr"),
                                measure.vars = c("Predicted", "Actual"), value.name = "Points_Added")
title <- "Predicted vs Actual Points Scored by Time Remaining in Quarter - Validation Data"
time.plot <- ggplot(data = val_TD_time2, aes(x = TimeUnder, y = Points_Added, colour = variable, group = variable)) + ggtitle(title) + 
  geom_line(aes(size = .2)) + geom_point(shape = 15) + facet_grid(qtr~.) + scale_size(range = c(.1, 1.5), guide = F) + scale_shape_discrete(solid = T)
print(time.plot)
dev.off()

# Save regression objects
filename <- paste(path_out, "Initial_Simple_OLS_EP_TN.rdata", sep="")
save(reg2_EP_TN, file = filename)
# Close Log
sink()
