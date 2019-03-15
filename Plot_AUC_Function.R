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