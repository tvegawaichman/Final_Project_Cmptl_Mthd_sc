## Load packages

set.seed(1234)
library(tidyverse)
library(caret)
library(ComplexHeatmap)

## Set functions 
get_pred = function(pred, tool, true){
  pred %>%
    select(tool) %>%
    mutate(label = .data[[tool]],
           label = ifelse(!label %in% true$label, NA, label),
           label = factor(label, ordered = TRUE)) %>%
    return()
}

get_pred_levels = function(pred, label_pred, label_true){
  true_label <- true[,label_true,drop=T]
  pred %>%
    select(label_pred) %>%
    mutate(label = .data[[label_pred]],
           label = ifelse(!label %in% true_label, NA, label),
           label = factor(label, ordered = TRUE)) %>%
    return()
}

# Plot confusion matrix as a heatmap 
plot_cm = function(cm_table){
  col_fun = circlize::colorRamp2(c(range(cm_table)[1], 
                                   range(cm_table)[2]/2, 
                                   range(cm_table)[2]), 
                                 c("#5C80BC", "#F2EFC7", "#FF595E")) 
  
  h = Heatmap(cm_table,
              name = 'Counts',
              col = col_fun,
              width = ncol(cm_table)*unit(2, "mm"),
              height = nrow(cm_table)*unit(2, "mm"),
              cluster_rows = F, 
              cluster_columns = F, 
              row_names_gp = gpar(fontsize = 7),
              column_names_gp = gpar(fontsize = 7), 
              column_title = 'True Class', 
              row_title = 'Predicted Class')
  
  return(h)
}

# Plot class stat per fold (F1 etc) as barplot
plot_stat = function(cm_byclass, stat){
  
  p = cm_byclass %>% 
    as.data.frame() %>%
    rownames_to_column('class') %>%
    separate(class, into = c(NA, 'class'), sep = ': ') %>%
    ggplot(aes(reorder(class, -.data[[stat]]), .data[[stat]])) +
    geom_bar(stat = 'identity', col = 'white', fill = 'lightgrey') +
    theme_bw() +
    theme(text = element_text(size = 10), 
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.5), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     vjust = 1, 
                                     hjust=1),
          aspect.ratio = 0.5) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_hline(yintercept = c(1, 0.5), linetype = 'dotted', color = 'red')
  
  return(p)
}

# plot F1 accross folds for each class as a boxplot  
plot_stat_boxplot = function(list, tool, stat){
  
  df = lapply(list[[tool]], get_stat, stat = stat) %>% bind_rows()
  
  df[is.na(df)] = 0
  
  df %>%
    ggplot(aes(reorder(class, -.data[[stat]], mean), .data[[stat]])) +
    geom_boxplot() +
    theme_bw() +
    theme(text = element_text(size = 10), 
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.5), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     vjust = 1, 
                                     hjust=1),
          aspect.ratio = 0.5) +
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0, 0)) +
    geom_hline(yintercept = c(1, 0.5), linetype = 'dotted', color = 'red')
}

# plot average stat for all tools 
plot_mean_tool = function(list, stat, tools){
  
  df = lapply(list, function(x){lapply(x, get_stat, stat = stat) %>% bind_rows()})
  
  df = bind_rows(df) %>% 
    group_by(class, tool) %>%
    mutate(mean = mean(.data[[stat]])) %>%
    distinct(class, tool, mean) %>% 
    pivot_wider(names_from = 'class', values_from = mean) %>%
    column_to_rownames('tool')
  
  df[is.na(df)] = 0
  
  col_fun = circlize::colorRamp2(c(0, 
                                   range(df)[2]/2, 
                                   range(df)[2]), 
                                 c("#3B5B91", "#F2EFC7", "#CC0007")) 
  
  split = c('Consensus', rep('tools', length(tools)-1))
  
  h = Heatmap(df,
              name = paste('Mean ', stat),
              col = col_fun,
              width = ncol(df)*unit(4, "mm"),
              height = nrow(df)*unit(6, "mm"),
              row_names_side = 'left',
              row_names_gp = gpar(fontsize = 12),
              show_column_dend = F,
              show_row_dend = F, 
              row_split = split,
              cluster_row_slices = F, 
              row_title = NULL)
  
  return(h)
}

plot_n_cells_per_class = function(df){
  mean_n = df %>%
    count(label, fold) %>%
    group_by(label) %>%
    summarise(mean = round(mean(n)))
  
  b = df %>%
    count(label, fold) %>%
    ggplot(aes(reorder(label, desc(mean)), mean)) +
    geom_bar(data = mean_n, 
             mapping = aes(reorder(label, desc(mean)), mean), 
             stat = 'identity', 
             fill = 'grey90') +
    geom_text(data = mean_n, mapping = aes(label = mean), hjust = -0.2, vjust = -0.2, angle = 45) +
    geom_point(aes(label, n), alpha = 0.5) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(mean_n$mean) + (max(mean_n$mean)*0.2))) +
    ylab('N') +
    theme(text = element_text(size = 10), 
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.5), 
          panel.border = element_blank(), 
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     vjust = 1, 
                                     hjust=1))
  
  return(b)
}

#--------- HELPER FUNCTIONS ----------------

# gets stat for each fold and returns data frame 
get_stat = function(x, stat){
  x$byClass %>% 
    as.data.frame() %>%
    rownames_to_column('class') %>%
    separate(class, into = c(NA, 'class'), sep = ': ') %>%
    select(class, .data[[stat]]) %>%
    mutate(fold = x$fold,
           tool = x$tool)
}


## Load ontology
# Read prediction and true labels for each tool and each fold and calculate confusion matrix and stats 
# Save everything in a list object with hierarchy TOOL > FOLD > STATS 
metadataLabelsPath  <- "/project/kleinman/selin.jessa/from_hydra/atlas/data/metadata_extended/metadata_20210710_with_qc_v2.tsv"
metadataLabels <- read_tsv(metadataLabelsPath)
#Get the labels
val <-metadataLabels$Label %>% str_split("_",n = 2) %>% sapply(function(x){tail(x,n=1)})

## Set global variables

tools = c("SVM","SVM_SCENIC")
fold = as.numeric(5)

### Heatmap Level 1 ----
list_Lvl1 = list()
for(n in 1:fold){
  
  # read true lables 
  true = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/test_labels.csv'), header = T) %>%
    column_to_rownames('V1') %>%
    mutate(label = factor(label, ordered = TRUE))
  
  true$label <- as.factor(true$label)
  levels(true$label) <- metadataLabels[match(levels(true$label),val),"Level1_type",drop=T]
  true$label <- as.character(true$label)
  # read prediction summary for fold
  pred = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/SVM/pred.csv'), header = T)%>%
    column_to_rownames('cell')
  colnames(pred) <- "SVM"
  
  pred$SVM <- as.factor(pred$SVM)
  levels(pred$SVM) <- metadataLabels[match(levels(pred$SVM),val),"Level1_type",drop=T]
  pred$SVM <- as.character(pred$SVM)
  pred_SCENIC = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/SVM/pred_SCENIC.csv'), header = T)%>%
    column_to_rownames('cell')
  colnames(pred_SCENIC) <- "SVM_SCENIC"
  pred_SCENIC$SVM_SCENIC <- as.factor(pred_SCENIC$SVM_SCENIC)
  levels(pred_SCENIC$SVM_SCENIC) <- metadataLabels[match(levels(pred_SCENIC$SVM_SCENIC),val),"Level1_type",drop=T]
  pred_SCENIC$SVM_SCENIC <- as.character(pred_SCENIC$SVM_SCENIC)
  pred <- cbind(pred,pred_SCENIC)
  for(t in tools){
    
    tmp = get_pred(pred, t, true)
    
    list_Lvl1[[t]][[n]] = confusionMatrix(data = tmp$label, reference = as.factor(true$label), mode = 'everything')
    list_Lvl1[[t]][[n]]$fold = paste0('fold', n)
    list_Lvl1[[t]][[n]]$tool = t
    
    #change na values to 0 
    list_Lvl1[[t]][[n]]$byClass[is.na(list_Lvl1[[t]][[n]]$byClass)] = 0
  }
}

# save F1 as table 
F1 = lapply(list_Lvl1, function(x){lapply(x, get_stat, stat = 'F1') %>% bind_rows()}) %>% bind_rows()
#data.table::fwrite(F1, file=paste0(params$pred_path, '/report/F1.csv'))

plot_mean_tool(list_Lvl1, 'F1', tools)

### Heatmap Level 2 ----
list_Lvl2 = list()
for(n in 1:fold){
  
  # read true lables 
  true = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/test_labels.csv'), header = T) %>%
    column_to_rownames('V1') %>%
    mutate(label = factor(label, ordered = TRUE))
  
  true$label <- as.factor(true$label)
  levels(true$label) <- metadataLabels[match(levels(true$label),val),"Level2_type",drop=T]
  true$label <- as.character(true$label)
  # read prediction summary for fold
  pred = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/SVM/pred.csv'), header = T)%>%
    column_to_rownames('cell')
  colnames(pred) <- "SVM"
  
  pred$SVM <- as.factor(pred$SVM)
  levels(pred$SVM) <- metadataLabels[match(levels(pred$SVM),val),"Level2_type",drop=T]
  pred$SVM <- as.character(pred$SVM)
  pred_SCENIC = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/SVM/pred_SCENIC.csv'), header = T)%>%
    column_to_rownames('cell')
  colnames(pred_SCENIC) <- "SVM_SCENIC"
  pred_SCENIC$SVM_SCENIC <- as.factor(pred_SCENIC$SVM_SCENIC)
  levels(pred_SCENIC$SVM_SCENIC) <- metadataLabels[match(levels(pred_SCENIC$SVM_SCENIC),val),"Level2_type",drop=T]
  pred_SCENIC$SVM_SCENIC <- as.character(pred_SCENIC$SVM_SCENIC)
  pred <- cbind(pred,pred_SCENIC)
  for(t in tools){
    
    tmp = get_pred(pred, t, true)
    
    list_Lvl2[[t]][[n]] = confusionMatrix(data = tmp$label, reference = as.factor(true$label), mode = 'everything')
    list_Lvl2[[t]][[n]]$fold = paste0('fold', n)
    list_Lvl2[[t]][[n]]$tool = t
    
    #change na values to 0 
    list_Lvl2[[t]][[n]]$byClass[is.na(list_Lvl2[[t]][[n]]$byClass)] = 0
  }
}

# save list object with all stats 
#save(list, file=paste0(params$pred_path, '/report/stats.Rda'))

# save F1 as table 
F1 = lapply(list_Lvl2, function(x){lapply(x, get_stat, stat = 'F1') %>% bind_rows()}) %>% bind_rows()
#data.table::fwrite(F1, file=paste0(params$pred_path, '/report/F1.csv'))

plot_mean_tool(list_Lvl2, 'F1', tools)

### Heatmap Level 3 ----
list_Lvl3 = list()
for(n in 1:fold){
  
  # read true lables 
  true = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/test_labels.csv'), header = T) %>%
    column_to_rownames('V1') %>%
    mutate(label = factor(label, ordered = TRUE))
  
  true$label <- as.factor(true$label)
  levels(true$label) <- metadataLabels[match(levels(true$label),val),"Level3_type",drop=T]
  true$label <- as.character(true$label)
  # read prediction summary for fold
  pred = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/SVM/pred.csv'), header = T)%>%
    column_to_rownames('cell')
  colnames(pred) <- "SVM"
  
  pred$SVM <- as.factor(pred$SVM)
  levels(pred$SVM) <- metadataLabels[match(levels(pred$SVM),val),"Level3_type",drop=T]
  pred$SVM <- as.character(pred$SVM)
  pred_SCENIC = data.table::fread(paste0('/project/kleinman/tomas.vegawaichman/from_hydra/Random/Tom/EXMD521/Final_Project/output/', '/fold', n, '/SVM/pred_SCENIC.csv'), header = T)%>%
    column_to_rownames('cell')
  colnames(pred_SCENIC) <- "SVM_SCENIC"
  pred_SCENIC$SVM_SCENIC <- as.factor(pred_SCENIC$SVM_SCENIC)
  levels(pred_SCENIC$SVM_SCENIC) <- metadataLabels[match(levels(pred_SCENIC$SVM_SCENIC),val),"Level3_type",drop=T]
  pred_SCENIC$SVM_SCENIC <- as.character(pred_SCENIC$SVM_SCENIC)
  pred <- cbind(pred,pred_SCENIC)
  for(t in tools){
    
    tmp = get_pred(pred, t, true)
    
    list_Lvl3[[t]][[n]] = confusionMatrix(data = tmp$label, reference = as.factor(true$label), mode = 'everything')
    list_Lvl3[[t]][[n]]$fold = paste0('fold', n)
    list_Lvl3[[t]][[n]]$tool = t
    
    #change na values to 0 
    list_Lvl3[[t]][[n]]$byClass[is.na(list_Lvl3[[t]][[n]]$byClass)] = 0
  }
}

# save F1 as table 
F1 = lapply(list_Lvl3, function(x){lapply(x, get_stat, stat = 'F1') %>% bind_rows()}) %>% bind_rows()
#data.table::fwrite(F1, file=paste0(params$pred_path, '/report/F1.csv'))

plot_mean_tool(list_Lvl3, 'F1', tools)

