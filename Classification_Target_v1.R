# Title: Classification_Target_v1.R
# Author: Aaron Weisberg
# Created: Feb 23, 2017
# Last Edit: Mar 2, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and explores the EP training data for relationhips
#   between points added and key scoring drivers in football using
#   a multi-level classification variable as the target

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
require(mgcv)
require(graphics)
require(glmnet)
require(reshape)
require(reshape2)

# Set file paths and clear data
rm(list=ls())
path_data <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"
path_log  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_out  <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Analysis\\"

# Set up Log File
log_filename <- paste(path_log, "Classification_Target_v1_",
                      gsub(":", "_", gsub("-", "_", gsub(" ", "_", Sys.Date()))), ".txt", sep="")
sink(file = log_filename, append = F, type = c("output", "message"), split = T)

# Bring in training and validation data sets
filename <- paste(path_data, "trn_data_post_deep_dive.rdata", sep="")
load(filename)
filename <- paste(path_data, "val_data_post_deep_dive.rdata", sep="")
load(filename)

# Separate PATs and remove timeout plays 
trn_data_PAT <- trn_data[(trn_data$ExPointResult != 'No Attempt' | trn_data$TwoPointConv != 'No Attempt' | trn_data$DefTwoPoint != 'No Attempt') & trn_data$PlayType != 'Timeout', ]
trn_data_TD <- trn_data[trn_data$ExPointResult == 'No Attempt' & trn_data$TwoPointConv == 'No Attempt' & trn_data$DefTwoPoint == 'No Attempt' & trn_data$PlayType != 'Timeout', ]
val_data_PAT <- val_data[(val_data$ExPointResult != 'No Attempt' | val_data$TwoPointConv != 'No Attempt' | val_data$DefTwoPoint != 'No Attempt') & val_data$PlayType != 'Timeout', ]
val_data_TD <- val_data[val_data$ExPointResult == 'No Attempt' & val_data$TwoPointConv == 'No Attempt' & val_data$DefTwoPoint == 'No Attempt' & val_data$PlayType != 'Timeout', ]

# Fix data anomaly - penalties before extra points that don't register as extra points for some reason
qc <- trn_data_TD[trn_data_TD$Points.Added == '1',]
trn_data_TD <- trn_data_TD[!(trn_data_TD$Play.ID %in% qc$Play.ID), ]
qc <- val_data_TD[val_data_TD$Points.Added == '1',]
val_data_TD <- val_data_TD[!(val_data_TD$Play.ID %in% qc$Play.ID), ]

# Set Seed
set.seed(12345)

# Turn Points.Added into Factor
trn_data_TD$Points.Added <- as.factor(trn_data_TD$Points.Added)
val_data_TD$Points.Added <- as.factor(val_data_TD$Points.Added)

# Turn down into Factor
trn_data_TD$down <- as.factor(trn_data_TD$down)
val_data_TD$down <- as.factor(val_data_TD$down)

# Set first formula based on PCA and random forest from before
formula1 <- as.formula(Points.Added ~ yrdline100 + TimeSecs + down + ydstogo + AbsScoreDiff + GoalToGo)
# Run New RandomForest as classification model - dep var is a factor
forest1 <- randomForest(formula = formula1, data = trn_data_TD, importance = T, ntree = 100, mtry = 4,
                        nodesize = 10, sampsize = 10000, na.action = na.omit)
varImpPlot(forest1)

print(forest1)

trn_pred <- predict(forest1, trn_data_TD, type = "prob")
val_pred <- predict(forest1, val_data_TD, type = "prob")

trn_data_TD$Pred.RF1.TD <- trn_pred[,7]
val_data_TD$Pred.RF1.TD <- val_pred[,7]

trn_data_TD$Pred.RF1.FG <- trn_pred[,6]
val_data_TD$Pred.RF1.FG <- val_pred[,6]

trn_data_TD$Pred.RF1.SF <- trn_pred[,5]
val_data_TD$Pred.RF1.SF <- val_pred[,5]

trn_data_TD$Pred.RF1.None <- trn_pred[,4]
val_data_TD$Pred.RF1.None <- val_pred[,4]

trn_data_TD$Pred.RF1.SFA <- trn_pred[,3]
val_data_TD$Pred.RF1.SFA <- val_pred[,3]

trn_data_TD$Pred.RF1.FGA <- trn_pred[,2]
val_data_TD$Pred.RF1.FGA <- val_pred[,2]

trn_data_TD$Pred.RF1.TDA <- trn_pred[,1]
val_data_TD$Pred.RF1.TDA <- val_pred[,1]

trn_data_TD <- trn_data_TD %>% mutate(Pred.RF1.EP = (Pred.RF1.TD * 6) + (Pred.RF1.FG * 3) +
                                        (Pred.RF1.SF * 2) + (Pred.RF1.None * 0)  +
                                        (Pred.RF1.SFA * -2) + (Pred.RF1.FGA * -3) +
                                        (Pred.RF1.TDA*-6))

val_data_TD <- val_data_TD %>% mutate(Pred.RF1.EP = (Pred.RF1.TD * 6) + (Pred.RF1.FG * 3) +
                                        (Pred.RF1.SF * 2) + (Pred.RF1.None * 0)  +
                                        (Pred.RF1.SFA * -2) + (Pred.RF1.FGA * -3) +
                                        (Pred.RF1.TDA*-6))
# Create binary indicator for every event 
trn_data_TD <- trn_data_TD %>% mutate(TD_Next_Score = ifelse(Points.Added == '6', 1, 0),
                                      FG_Next_Score = ifelse(Points.Added == '3', 1, 0),
                                      SF_Next_Score = ifelse(Points.Added == '2', 1, 0),
                                      No_Next_Score = ifelse(Points.Added == '0', 1, 0),
                                      SFA_Next_Score = ifelse(Points.Added == '-2', 1, 0),
                                      FGA_Next_Score = ifelse(Points.Added == '-3', 1, 0),
                                      TDA_Next_Score = ifelse(Points.Added == '-6', 1, 0))

val_data_TD <- val_data_TD %>% mutate(TD_Next_Score = ifelse(Points.Added == '6', 1, 0),
                                      FG_Next_Score = ifelse(Points.Added == '3', 1, 0),
                                      SF_Next_Score = ifelse(Points.Added == '2', 1, 0),
                                      No_Next_Score = ifelse(Points.Added == '0', 1, 0),
                                      SFA_Next_Score = ifelse(Points.Added == '-2', 1, 0),
                                      FGA_Next_Score = ifelse(Points.Added == '-3', 1, 0),
                                      TDA_Next_Score = ifelse(Points.Added == '-6', 1, 0))

# See how well random forest predicts across our categories using AUC and ROC curve
plot.AUC <- function(trn_data_pred, trn_data_actual, val_data_pred, val_data_actual, title){
  # Get prediction and performance objects for training and validation
  trn_pred <- prediction(trn_data_pred, trn_data_actual)
  val_pred <- prediction(val_data_pred, val_data_actual)
  trn_perf <- performance(trn_pred, 'tpr', 'fpr')
  val_perf <- performance(val_pred, 'tpr', 'fpr')
  # Get AUC for training and validation
  trn.auc <- performance(trn_pred, 'auc')
  val.auc <- performance(val_pred, 'auc')
  # Store true positive and false positive rates in vectors
  trn.tpr <- trn_pred@tp[[1]]/max(trn_pred@tp[[1]])
  val.tpr <- val_pred@tp[[1]]/max(val_pred@tp[[1]])
  trn.fpr <- trn_pred@fp[[1]]/max(trn_pred@fp[[1]])
  val.fpr <- val_pred@fp[[1]]/max(val_pred@fp[[1]])
  # Store actual AUC values
  trn.auc2 <- trn.auc@y.values[[1]]
  val.auc2 <- val.auc@y.values[[1]]
  
  trn.data <- data.frame(cbind(trn.tpr, trn.fpr))
  val.data <- data.frame(cbind(val.tpr, val.fpr))
  trn.data$dataset <- "Training"
  val.data$dataset <- "Validation"
  
  colnames(trn.data) <- c("tpr", "fpr", "dataset")
  colnames(val.data) <- c("tpr", "fpr", "dataset")
  
  graph.data <- rbind(trn.data, val.data)
  colnames(graph.data) <- c("tpr", "fpr", "dataset")
  
  graph <- ggplot(data = graph.data, aes(x = fpr, y = tpr, color = dataset)) + ggtitle(title) +
    geom_line() + geom_abline(intercept = 0, slope = 1) + 
    annotate("text", label = paste("Training AUC = ", percent(trn.auc2), sep=""), x = .8, y = .4) + 
    annotate("text", label = paste("Validation AUC = ", percent(val.auc2), sep=""), x = .8, y = .2) +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) +
    xlab("False Positive Rate") + ylab("True Positive Rate")
  print(graph)
  return(graph)
}
# TD For Performance
title <- "AUC - Random Forest - TD for vs no TD for"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.RF1.TD, trn_data_TD$TD_Next_Score, val_data_TD$Pred.RF1.TD, val_data_TD$TD_Next_Score, title)
dev.off()
# FG for Performance
title <- "AUC - Random Forest - FG for vs no FG for"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.RF1.FG, trn_data_TD$FG_Next_Score, val_data_TD$Pred.RF1.FG, val_data_TD$FG_Next_Score, title)
dev.off()
# Safety For Performance
title <- "AUC - Random Forest - Safety for vs no Safety for"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.RF1.SF, trn_data_TD$SF_Next_Score, val_data_TD$Pred.RF1.SF, val_data_TD$SF_Next_Score, title)
dev.off()

# TD Against Performance
title <- "AUC - Random Forest - TD Against vs no TD Against"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.RF1.TDA, trn_data_TD$TDA_Next_Score, val_data_TD$Pred.RF1.TDA, val_data_TD$TDA_Next_Score, title)
dev.off()
# FG AGainst Performance
title <- "AUC - Random Forest - FG Against vs no FG Against"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.RF1.FGA, trn_data_TD$FGA_Next_Score, val_data_TD$Pred.RF1.FGA, val_data_TD$FGA_Next_Score, title)
dev.off()
# Safety Against Performance
title <- "AUC - Random Forest - Safety Against vs no Safety Against"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.RF1.SFA, trn_data_TD$SFA_Next_Score, val_data_TD$Pred.RF1.SFA, val_data_TD$SFA_Next_Score, title)
dev.off()

# Ok this thing does a pretty good job of discriminating an event from a non-event
# We also need to check how good it does at predicting the scoring
# Can check RMSE and RSQ using Points.Added vs EP
trn_data_TD$Points.Added <- as.numeric(levels(trn_data_TD$Points.Added))[trn_data_TD$Points.Added]
table(trn_data_TD$Points.Added)
R <- cor(trn_data_TD$Pred.RF1.EP, trn_data_TD$Points.Added)
R2 <- R^2
trn_data_TD$Points.Error <- trn_data_TD$Pred.RF1.EP - trn_data_TD$Points.Added
trn_data_TD$Points.Error.Sqrd <- (trn_data_TD$Points.Error)^2
MSE <- mean(trn_data_TD$Points.Error.Sqrd)
RMSE <- sqrt(MSE)
print("Random Forest Error Stats - Points Added")
MSE
RMSE
R2

# This isn't bad. In fact it's better than before.
# Lets take a crack at a multinomial regression model just to see how that does
trn_data_TD$Points.Added <- as.factor((trn_data_TD$Points.Added))
trn_data_TD$Points.Added <- relevel(trn_data_TD$Points.Added, ref = '0')
# Run multinomial
multinomial1 <- nnet::multinom(formula = formula1, data = trn_data_TD)
summary(multinomial1)
trn.multinom.pred <- predict(multinomial1, trn_data_TD, type = "probs")
val.multinom.pred <- predict(multinomial1, val_data_TD, type = "probs")

trn_data_TD$Pred.Multinom.None <- trn.multinom.pred[ , 1]
trn_data_TD$Pred.Multinom.TDA <- trn.multinom.pred[ , 2]
trn_data_TD$Pred.Multinom.FGA <- trn.multinom.pred[ , 3]
trn_data_TD$Pred.Multinom.SFA <- trn.multinom.pred[ , 4]
trn_data_TD$Pred.Multinom.SF <- trn.multinom.pred[ , 5]
trn_data_TD$Pred.Multinom.FG <- trn.multinom.pred[ , 6]
trn_data_TD$Pred.Multinom.TD <- trn.multinom.pred[ , 7]

val_data_TD$Pred.Multinom.None <- val.multinom.pred[ , 1]
val_data_TD$Pred.Multinom.TDA <- val.multinom.pred[ , 2]
val_data_TD$Pred.Multinom.FGA <- val.multinom.pred[ , 3]
val_data_TD$Pred.Multinom.SFA <- val.multinom.pred[ , 4]
val_data_TD$Pred.Multinom.SF <- val.multinom.pred[ , 5]
val_data_TD$Pred.Multinom.FG <- val.multinom.pred[ , 6]
val_data_TD$Pred.Multinom.TD <- val.multinom.pred[ , 7]

# Create binary indicator for every event 
trn_data_TD <- trn_data_TD %>% mutate(TD_Next_Score = ifelse(Points.Added == '6', 1, 0),
                                      FG_Next_Score = ifelse(Points.Added == '3', 1, 0),
                                      SF_Next_Score = ifelse(Points.Added == '2', 1, 0),
                                      No_Next_Score = ifelse(Points.Added == '0', 1, 0),
                                      SFA_Next_Score = ifelse(Points.Added == '-2', 1, 0),
                                      FGA_Next_Score = ifelse(Points.Added == '-3', 1, 0),
                                      TDA_Next_Score = ifelse(Points.Added == '-6', 1, 0))

val_data_TD <- val_data_TD %>% mutate(TD_Next_Score = ifelse(Points.Added == '6', 1, 0),
                                      FG_Next_Score = ifelse(Points.Added == '3', 1, 0),
                                      SF_Next_Score = ifelse(Points.Added == '2', 1, 0),
                                      No_Next_Score = ifelse(Points.Added == '0', 1, 0),
                                      SFA_Next_Score = ifelse(Points.Added == '-2', 1, 0),
                                      FGA_Next_Score = ifelse(Points.Added == '-3', 1, 0),
                                      TDA_Next_Score = ifelse(Points.Added == '-6', 1, 0))


# TD For Performance
title <- "AUC - Multinomial Regression - TD for vs no TD for"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.Multinom.TD, trn_data_TD$TD_Next_Score, val_data_TD$Pred.Multinom.TD, val_data_TD$TD_Next_Score, title)
dev.off()
# FG for Performance
title <- "AUC - Multinomial Regression - FG for vs no FG for"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.Multinom.FG, trn_data_TD$FG_Next_Score, val_data_TD$Pred.Multinom.FG, val_data_TD$FG_Next_Score, title)
dev.off()
# Safety For Performance
title <- "AUC - Multinomial Regression - Safety for vs no Safety for"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.Multinom.SF, trn_data_TD$SF_Next_Score, val_data_TD$Pred.Multinom.SF, val_data_TD$SF_Next_Score, title)
dev.off()

# TD Against Performance
title <- "AUC - Multinomial Regression - TD Against vs no TD Against"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.Multinom.TDA, trn_data_TD$TDA_Next_Score, val_data_TD$Pred.Multinom.TDA, val_data_TD$TDA_Next_Score, title)
dev.off()
# FG AGainst Performance
title <- "AUC - Multinomial Regression - FG Against vs no FG Against"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.Multinom.FGA, trn_data_TD$FGA_Next_Score, val_data_TD$Pred.Multinom.FGA, val_data_TD$FGA_Next_Score, title)
dev.off()
# Safety Against Performance
title <- "AUC - Multinomial Regression - Safety Against vs no Safety Against"
png.filename <- paste(path_out, title,"_", gsub("-", "_", Sys.Date()), ".png", sep = "")
png(filename = png.filename)
plot.AUC(trn_data_TD$Pred.Multinom.SFA, trn_data_TD$SFA_Next_Score, val_data_TD$Pred.Multinom.SFA, val_data_TD$SFA_Next_Score, title)
dev.off()

# Check overall EP error
trn_data_TD <- trn_data_TD %>% mutate(Pred.Multinom.EP = (Pred.Multinom.TD * 6) + (Pred.Multinom.FG * 3) +
                                        (Pred.Multinom.SF * 2) + (Pred.Multinom.None * 0)  +
                                        (Pred.Multinom.SFA * -2) + (Pred.Multinom.FGA * -3) +
                                        (Pred.Multinom.TDA*-6))

val_data_TD <- val_data_TD %>% mutate(Pred.Multinom.EP = (Pred.Multinom.TD * 6) + (Pred.Multinom.FG * 3) +
                                        (Pred.Multinom.SF * 2) + (Pred.Multinom.None * 0)  +
                                        (Pred.Multinom.SFA * -2) + (Pred.Multinom.FGA * -3) +
                                        (Pred.Multinom.TDA*-6))
trn_data_TD$Points.Added <- as.numeric(levels(trn_data_TD$Points.Added))[trn_data_TD$Points.Added]
table(trn_data_TD$Points.Added)
R <- cor(trn_data_TD$Pred.Multinom.EP, trn_data_TD$Points.Added)
R2 <- R^2
trn_data_TD$Points.Error <- trn_data_TD$Pred.Multinom.EP - trn_data_TD$Points.Added
trn_data_TD$Points.Error.Sqrd <- (trn_data_TD$Points.Error)^2
MSE <- mean(trn_data_TD$Points.Error.Sqrd)
RMSE <- sqrt(MSE)
print("Multinomial Regression Error Stats - Points Added")
MSE
RMSE
R2

# These both look pretty good. Lets save the model objects so we have them for later
filename <- paste(path_data, "Classification_Forest1.rdata", sep="")
save(forest1, file = filename)
filename <- paste(path_data, "Classification_Multinom1.rdata", sep="")
save(multinomial1, file = filename)

# Close Log
sink()
