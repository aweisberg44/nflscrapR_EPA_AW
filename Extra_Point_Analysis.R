# Title: Extra_Point_Analysis.R
# Author: Aaron Weisberg
# Created: Mar 2, 2017
# Last Edit: Mar 2, 2017
# Description: This script takes NFL play by play data
#   from 2009 to 2016 and analyzes extra points and
#   two point conversions.


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
log_filename <- paste(path_log, "Extra_Point_Analysis",
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

# Lets check the conversion rates for extra points and 2 point conversions
table(trn_data_PAT$ExPointResult)
table(trn_data_PAT$TwoPointConv)

ex.point.rate <- 4463/(4463+26+45)
two.point.rate <- 118/(118+131)

#Check validation rate for safety
table(val_data_PAT$ExPointResult)
table(val_data_PAT$TwoPointConv)

check.epr.val <- 3315 / (3315 + 21 + 30)
check.tpr.val <- 85 / (85 + 95)

# That extra point rate doesn't factor in the rule change when kicks were moved back
# Lets re-do that rate for 2015 and after and see what it says
trn_data_PAT_2015 <- trn_data_PAT[trn_data_PAT$Season == 2015, ]
table(trn_data_PAT_2015$ExPointResult)
ex.point.rate.2015 <- 583 / (583 + 7 + 34)
val_data_PAT_2015 <- val_data_PAT[val_data_PAT$Season == 2015, ]
table(val_data_PAT_2015$ExPointResult)
check.erp.val.2015 <- 482 / (482 + 4 + 21)
# That 93% rate makes a lot more sense to me
# Is it worth it to model extra points? why not just take EP = .93?
# Validation rates are pretty close to the training rates
# Not sure if extra points should really be modeled with this data
# Dito for 2 point conversions
# Same for 2 point conversions - why not just take EP = .47 * 2 = .94?
# I think that is the best way to do this. Makes the model pretty simple
# Save objects for later scoring
filename <- paste(path_data, "extra_point_rate_post_rule_change.rdata", sep = "")
save(ex.point.rate.2015, file = filename)
filename <- paste(path_data, "two_point_conv_rate.rdata", sep="")
save(two.point.rate, file = filename)
# Close Log
sink()