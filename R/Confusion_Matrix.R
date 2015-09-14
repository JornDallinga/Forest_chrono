Confusion_Matrix <- function(Sexton, Hansen){
  
  Sexton <- as.matrix(Sexton)
  Hansen <- as.matrix(Hansen)
  
  Reference <- matrix(Sexton)
  Predicted <- matrix(Hansen)
  
  Reference[Reference == 1] <- "Non.Forest"
  Reference[Reference == 2] <- "Forest"
  Reference[Reference == 3] <- "Water"
  Reference[Reference == 4] <- "Clouds/Shadow/No_data"
  
  Predicted[Predicted == 1] <- "Non.Forest"
  Predicted[Predicted == 2] <- "Forest"
  Predicted[Predicted == 3] <- "Water"
  Predicted[Predicted == 4] <- "Clouds/Shadow/No_data"
  
  Sexton <- Reference
  Hansen <- Predicted
  
  tabletest <- table(Hansen,Sexton)
  
  if ((nrow(tabletest) == ncol(tabletest)) & ((nrow(tabletest) & ncol(tabletest)) != 1)){
    outcome <- confusionMatrix(tabletest)
    
    Confusion_table <- as.data.frame.matrix(outcome$table)
    Confusion_overall <- as.matrix(outcome$overall)
    
    dir.create(file.path('output/Excel/Confusion_Matrix'), showWarnings = FALSE)
    dir.create(file.path(sprintf('output/Excel/Confusion_Matrix/Buffer_%s', BufferDistance)), showWarnings = FALSE)
    dir.create(file.path(sprintf('output/Excel/Confusion_Matrix/Buffer_%s/Threshold_%s', BufferDistance, Threshold)), showWarnings = FALSE)
    write.xlsx(Confusion_table, file = sprintf("output/Excel/Confusion_Matrix/Buffer_%s/Threshold_%s/Con_%s_Buffer%s_Threshold%s.xlsx", BufferDistance, Threshold, Chronosequence, BufferDistance, Threshold), sheetName = "Confusion_Matrix")
    write.xlsx(Confusion_overall, file = sprintf("output/Excel/Confusion_Matrix/Buffer_%s/Threshold_%s/Con_%s_Buffer%s_Threshold%s.xlsx", BufferDistance, Threshold, Chronosequence, BufferDistance, Threshold), sheetName = "Confusion_Overall", append = T)
  
  } else {
    
  }
 
}

