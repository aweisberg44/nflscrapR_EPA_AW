# Title: Additional_Model_Build_EP_Team_Neutral.R
# Author: Aaron Weisberg
# Created: Feb 19, 2017
# Last Edit: Feb 23, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and explores the EP training data for relationhips
#   between points added and key scoring drivers in football using
#   more advanced statistical methods and variable transformations
#   to build off of the initial regression in Initial_Model_build_EP_Team_Neutral.R


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
log_filename <- paste(path_log, "Additional_Model_Build_EP_Team_Neutral_",
                      gsub(":", "_", gsub("-", "_", gsub(" ", "_", Sys.Date()))), ".txt", sep="")
sink(file = log_filename, append = F, type = c("output", "message"), split = T)

# Bring in training and validation data sets
filename <- paste(path_data, "trn_data_post_deep_dive.rdata", sep="")
load(filename)
filename <- paste(path_data, "val_data_post_deep_dive.rdata", sep="")
load(filename)

# Ok we did a simple OLS regression last time. We noticed a few different things when we checked out the model performance:
# 0. R-squared was not bad (roughly 14% both in and out of sample), but RMSE was around 4 points added  These can be improved 
#     via more advanced modeling techniques and data transformations on our important fields.
# 1. No Down has poor fit both in and out of sample across several dimensions. Likely has to do with a lot of these being kickoffs.
# 2. End of quarters has pretty bad fit. May need to try some transformations, bucketing, or indicator variable for this in OLS
# 3. No cross validation used. I will use when evaluating final champion team neutral expected points model. Gotta do this at some point
# 4. There are several more ways to evaluate effectiveness of EP model besides the few simple cuts I looked at previously.  I will build
#     several more ways to evaluate the model's performance for testing in the future.

# Lets try a random forest with our same small set of variables we initially used in the regression models and see
#   if we can get a similar/better R-squared with a smaller RMSE.
# Lets also re-include AbsScoreDiff in formula - PCA and Random Forest liked those variables initialy
trn_data_PAT <- trn_data[trn_data$ExPointResult != 'No Attempt' | trn_data$TwoPointConv != 'No Attempt' | trn_data$DefTwoPoint != 'No Attempt', ]
trn_data_TD <- trn_data[trn_data$ExPointResult == 'No Attempt' & trn_data$TwoPointConv == 'No Attempt' & trn_data$DefTwoPoint == 'No Attempt', ]
val_data_PAT <- val_data[val_data$ExPointResult != 'No Attempt' | val_data$TwoPointConv != 'No Attempt' | val_data$DefTwoPoint != 'No Attempt', ]
val_data_TD <- val_data[val_data$ExPointResult == 'No Attempt' & val_data$TwoPointConv == 'No Attempt' & val_data$DefTwoPoint == 'No Attempt', ]

formula1 <- as.formula(Points.Added ~ yrdline100 + TimeSecs + down + ydstogo + AbsScoreDiff + GoalToGo)
set.seed(12345)
trn_data_TD$down <- as.factor(trn_data_TD$down)
trn_data_TD$down <- relevel(trn_data_TD$down, ref = 1)
val_data_TD$down <- as.factor(val_data_TD$down)
val_data_TD$down <- relevel(val_data_TD$down, ref = 1)
# forestvars <- c("Points.Added", "yrdline100", "TimeSecs", "down", "ydstogo", "AbsScoreDiff", "GoalToGo")
# forestdata <- trn_data_TD[ , forestvars]
forest1 <- randomForest(formula = formula1, data = trn_data_TD, importance = T, ntree = 100, 
                        nodesize = 10, sampsize = 10000, na.action = na.omit)
varImpPlot(forest1)
# Use forest to get predicted points added
trn_data_TD$Pred.RF1 <- predict(forest1, trn_data_TD)
val_data_TD$Pred.RF1 <- predict(forest1, val_data_TD)

# Check R-squared and RMSE for first random forest
postResample(pred = trn_data_TD$Pred.RF1, obs = trn_data_TD$Points.Added)
postResample(pred = val_data_TD$Pred.RF1, obs = val_data_TD$Points.Added)

# This is kinda disappointing. In sample is great, but out of sample isn't as stong as far as RMSE and R-Squared...
# Still better than original regression though
# Cross validate later
# Food for thought - Using a forest may actually require more variables to work properly on out of sample data
# Lets table the forest idea for now and try some other regression based methods instead
# Next up - generalized additive model
trn_data_TD <- trn_data_TD %>% mutate(down2 = ifelse(down == 'No Down', 0, down))
trn_data_TD$down2 <- as.numeric(trn_data_TD$down2)
val_data_TD <- val_data_TD %>% mutate(down2 = ifelse(down == 'No Down', 0, down))
val_data_TD$down2 <- as.numeric(val_data_TD$down2)
gam1_EP_TN <- gam(Points.Added ~ s(yrdline100) + s(TimeSecs) + te(down2, ydstogo) + s(AbsScoreDiff),
            data=trn_data_TD, na.action = na.omit)

# Use GAM to get predicted points added
trn_data_TD$Pred.Gam1 <- predict(gam1_EP_TN, trn_data_TD)
val_data_TD$Pred.Gam1 <- predict(gam1_EP_TN, val_data_TD)

# Check R-squared and RMSE for first random forest
postResample(pred = trn_data_TD$Pred.Gam1, obs = trn_data_TD$Points.Added)
postResample(pred = val_data_TD$Pred.Gam1, obs = val_data_TD$Points.Added)
# Not bad, but not a super meaningful improvmement over simple OLS.  GAM also sacrifices interprettabiility a little bit as well
# This one is at least worth cross validating later along with the original regression
# Could also remove tensor product of down and distance if necessary
# Lets try LASSO regression next
LASSO.In <- c("yrdline100", "TimeSecs", "down2", "ydstogo", "AbsScoreDiff", "GoalToGo")
LASSO.Data <- as.matrix(trn_data_TD[ , LASSO.In])
LASSO.Dep <- c("Points.Added")
LASSO.Responses <- trn_data_TD$Points.Added

#Set up to try and minimize mean square error
LASSO1_EP_TN <- cv.glmnet(x = LASSO.Data, y = as.vector(LASSO.Responses), family = "gaussian", alpha = 1, nlambda = 20,
                          nfolds = 10, type.measure = "mse")
LASSO1_EP_TN
# Plot lambda values
plot(LASSO1_EP_TN, xvar = "lambda", label = T)
# Check coefficients for lambda that minimizes mean squared error
coef(LASSO1_EP_TN, s = "lambda.min")

# Predict based on lambda that minimizes mean squared error
trn_data_TD$Pred.LASSO1 <- predict(LASSO1_EP_TN, as.matrix(trn_data_TD[ , LASSO.In]), s ="lambda.min")
val_data_TD$Pred.LASSO1 <- predict(LASSO1_EP_TN, as.matrix(val_data_TD[ , LASSO.In]), s ="lambda.min")

# R-squared and RMSE based on lambda that minimizes mean squared error
postResample(pred = trn_data_TD$Pred.LASSO1, obs = trn_data_TD$Points.Added)
postResample(pred = val_data_TD$Pred.LASSO1, obs = val_data_TD$Points.Added)
# R-squared is awful and RMSE is pretty bad as well. Lets check it out if we use it on s = lambda.1se
# Check coefficients for lambda 1 standard error below mean
coef(LASSO1_EP_TN, s = "lambda.1se")

# Predict based on lambda that minimizes mean squared error
trn_data_TD$Pred.LASSO2 <- predict(LASSO1_EP_TN, as.matrix(trn_data_TD[ , LASSO.In]), s ="lambda.1se")
val_data_TD$Pred.LASSO2 <- predict(LASSO1_EP_TN, as.matrix(val_data_TD[ , LASSO.In]), s ="lambda.1se")

# R-squared and RMSE based on lambda that minimizes mean squared error
postResample(pred = trn_data_TD$Pred.LASSO2, obs = trn_data_TD$Points.Added)
postResample(pred = val_data_TD$Pred.LASSO2, obs = val_data_TD$Points.Added)
# Ok LASSO kinda sucks for this problem. Do not use.  As of right now, best methods appear to be OLS, gam, and random forest
# We should run cross validation on all those methods to see which is best as of right now
# We should also look at transforming some variables certain ways to increase fit
# We could also swap in ScoreDiff for AbsScoreDiff since it implies the direction of the margin
# Run cross validation via k-fold testing where each season in the data is a fold
# Load original regression
reg.filename <- paste(path_out, "Initial_Simple_OLS_EP_TN.rdata", sep="")
load(reg.filename)
# Ok set up a loop to run k-fold testing
perfstats <- NULL
for(year in 2009:2015){
  # Create scoring data
  score.year.trn <- trn_data_TD[trn_data_TD$Season == year, ]
  score.year.val <- val_data_TD[val_data_TD$Season == year, ]
  # Score with OLS regression
  score.year.trn$pred.reg <- predict(reg2_EP_TN, newdata = score.year.trn, type = 'response')
  score.year.val$pred.reg <- predict(reg2_EP_TN, newdata = score.year.val, type = 'response')
  reg.stats.trn <- as.data.frame(postResample(pred = score.year.trn$pred.reg, obs = score.year.trn$Points.Added))
  reg.stats.val <- as.data.frame(postResample(pred = score.year.val$pred.reg, obs = score.year.val$Points.Added))
  reg.stats.trn$method <- "OLS_Regression"
  reg.stats.val$method <- "OLS_Regression"
  reg.stats.trn$Sample <- "Training"
  reg.stats.val$Sample <- "Validation"
  reg.stats.trn$Season <- year
  reg.stats.val$Season <- year
  # Score with random forest
  score.year.trn$pred.forest <- predict(forest1, newdata = score.year.trn, type = 'response')
  score.year.val$pred.forest <- predict(forest1, newdata = score.year.val, type = 'response')
  forest.stats.trn <- as.data.frame(postResample(pred = score.year.trn$pred.forest, obs = score.year.trn$Points.Added))
  forest.stats.val <- as.data.frame(postResample(pred = score.year.val$pred.forest, obs = score.year.val$Points.Added))
  forest.stats.trn$method <- "Random_Forest"
  forest.stats.val$method <- "Random_Forest"
  forest.stats.trn$Sample <- "Training"
  forest.stats.val$Sample <- "Validation"
  forest.stats.trn$Season <- year
  forest.stats.val$Season <- year
  # Score with GAM
  score.year.trn$pred.gam <- predict(gam1_EP_TN, newdata = score.year.trn, type = 'response')
  score.year.val$pred.gam <- predict(gam1_EP_TN, newdata = score.year.val, type = 'response')
  gam.stats.trn <- as.data.frame(postResample(pred = score.year.trn$pred.gam, obs = score.year.trn$Points.Added))
  gam.stats.val <- as.data.frame(postResample(pred = score.year.val$pred.gam, obs = score.year.val$Points.Added))
  gam.stats.trn$method <- "GAM"
  gam.stats.val$method <- "GAM"
  gam.stats.trn$Sample <- "Training"
  gam.stats.val$Sample <- "Validation"
  gam.stats.trn$Season <- year
  gam.stats.val$Season <- year
  
  #Coerce Everything to have same column names
  column.names <- c("Value", "Method", "Sample" ,"Season")
  colnames(reg.stats.trn) <- column.names
  colnames(reg.stats.val) <- column.names
  colnames(forest.stats.trn) <- column.names
  colnames(forest.stats.val) <- column.names
  colnames(gam.stats.trn) <- column.names
  colnames(gam.stats.val) <- column.names
  
  reg.stats.trn$statistic <- rownames(reg.stats.trn) 
  reg.stats.val$statistic <- rownames(reg.stats.val)
  forest.stats.trn$statistic <- rownames(forest.stats.trn) 
  forest.stats.val$statistic <- rownames(forest.stats.val)
  gam.stats.trn$statistic <- rownames(gam.stats.trn) 
  gam.stats.val$statistic <- rownames(gam.stats.val)
  
  perfstats <- rbind(perfstats, reg.stats.trn, reg.stats.val, forest.stats.trn, forest.stats.val, gam.stats.trn, gam.stats.val)
  
}

# Visualize Performance Stats
filename1 <- paste(path_out, "RMSE Comparison - Initial Model Specs - OLS vs GAM vs Forest", gsub("-", "_", Sys.Date()), ".png",sep="")
png(filename = filename1)
title <- 'RMSE by Method and Season'
perfstats.rmse <- perfstats[perfstats$statistic == 'RMSE', ]
rmse <- ggplot(data=perfstats.rmse, aes(x = Season, y = Value, fill = Method)) + ggtitle(title) +
  geom_bar(stat = "identity", position = position_dodge()) + facet_grid(Sample~.)
rmse
dev.off()
filename1 <- paste(path_out, "R-Squared Comparison - Initial Model Specs - OLS vs GAM vs Forest", gsub("-", "_", Sys.Date()), ".png",sep="")
png(filename = filename1)
title <- 'R-Squared by Method and Season'
perfstats.rsq <- perfstats[perfstats$statistic == 'Rsquared', ]
rsq <- ggplot(data=perfstats.rsq, aes(x = Season, y = Value, fill = Method)) + ggtitle(title) +
  geom_bar(stat = "identity", position = position_dodge()) + facet_grid(Sample~.)
rsq

# These errors suck. Maybe we should switch to multinomial/classification model structure...
# RMSE of 4 points is garbage. R-squared of 14-16% is not bad
# Must be because target is not continuous even though it is not traditional binary/classification variable
dev.off()

# Random Forest looks a lot better
# Close Log File
sink()