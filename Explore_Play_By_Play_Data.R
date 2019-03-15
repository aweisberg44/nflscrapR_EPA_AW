# Title: Explore_Play_By_Play_Data.R
# Author: Aaron Weisberg
# Created: January 28, 2017
# Last Edit: January 28, 2017
# Description: Explore play by play data set with
#   every play in the NFL from 2009 to 2016

# Load Required Packages
require(nflscrapR)
require(dplyr)
require(tidyr)
require(plyr)
require(ggplot2)
require(utils)
require(scales)

rm(list=ls())


# Set paths for log and data export
path_log <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Log_Files\\"
path_data <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Data\\"
path_analysis <- "C:\\Users\\aweis\\OneDrive\\Documents\\NFL_Analytics\\Analysis\\"

# Set Up log File
log_file <- paste(path_log, "PBP_Master_Data_Explore_", gsub(" ", "_", gsub(":", " ", gsub("-", ":", Sys.time()))), ".txt", sep = "")
sink(file = log_file, append = F, type = c("output", "message"), split = T)

# Load play by play master data set
filename <- paste(path_data, "All_Play_By_Play_Data_2009_2016.rdata", sep="")
load(filename)

# Lets look at the full list of field names
names(master_data)
field_names <- names(master_data)

# Ok we have a lot of fields
# Lets take a look at them and get some basic summary data
summary(master_data)
# Numeric fields first
fields_numeric <- sapply(master_data, is.numeric)
numeric_data <- master_data[, fields_numeric]
names_numeric <- names(numeric_data)

# Lets see some very basic boxplots and histograms of the numeric data for data vis
pdf_filename <- paste(path_analysis, "Numeric_Fields_Distribution", gsub("-", "_", Sys.Date()), ".pdf", sep="")
pdf(file=pdf_filename)
for(i in 1:length(names_numeric)){
  var_to_plot <- names_numeric[i]
  
  #Boxplot
  title <- paste("Box plot of", var_to_plot)
  graphing_data <- numeric_data[ , var_to_plot]
  box <- boxplot(graphing_data, horizontal = T, main = title)
  
  #Histogram
  title <- paste("Histogram of", var_to_plot)
  histogram <- hist(graphing_data, main = title)
  
}
dev.off()

# Ok now lets get some basic information about what values our non-numeic fields take
fields_factor <- sapply(master_data, is.factor)
fields_char <- sapply(master_data, is.character)
data_factor <- master_data[ , fields_factor]
data_char <- master_data[ , fields_char]
non_numeric_data <- cbind(data_char, data_factor)
names_non_numeric <- names(non_numeric_data)

pdf_filename <- paste(path_analysis, "Non_Numeric_Fields_Distribution", gsub("-", "_", Sys.Date()), ".pdf", sep="")
pdf(file=pdf_filename)
for(i in 1:length(names_non_numeric)){
  var_to_plot <- names_non_numeric[i]
  graphing_data <- data.frame(table(non_numeric_data[ , var_to_plot]))
  
  # Bar chart with counts
  title <- paste("Bar Chart with Counts of", var_to_plot)
  g <- ggplot(data=graphing_data, aes(x=Var1, y=Freq)) + ggtitle(title) + geom_bar(stat="identity") +
    scale_y_continuous(labels = comma) + coord_flip()
  print(g)
}
dev.off()

#End Log
sink()