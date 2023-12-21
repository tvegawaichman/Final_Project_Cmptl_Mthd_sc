library(tidyverse)
library(data.table)
library(glue)

for(i in 1:5){
  test_auc_mat  <- read_csv(glue(path_test))
  train_auc_mat <- read_csv(glue(path_train))
  common_reg <- intersect(colnames(test_auc_mat),colnames(train_auc_mat))
  test_auc_mat <- test_auc_mat[,common_reg]
  colnames(test_auc_mat)[1] <- ""
  train_auc_mat <- train_auc_mat[,common_reg]
  colnames(train_auc_mat)[1] <- ""
  #Write count matrix
  fwrite(test_auc_mat,
         file= glue('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/fold{i}/test_SCENIC.csv'),
         row.names = F,
         col.names = T,
         sep = ",")
  
  #Write count matrix
  fwrite(train_auc_mat,
         file= glue('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/fold{i}/train_SCENIC.csv'),
         row.names = F,
         col.names = T,
         sep = ",")
}

