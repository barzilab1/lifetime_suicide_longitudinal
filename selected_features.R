
# all_features <- read_csv("all_p.csv")

demographics_features = c("bblid","sex","age","race2_White","race2_Black","goassessPhqDurMonths")

features_list = list()
lasso_selected_features = all_features[order(all_features$rank_lasso, decreasing = TRUE),c("feature","rank_lasso")]
features_list$lasso = lasso_selected_features$feature[1:13]

relieff_selected_features = all_features[order(all_features$rank_Relieff, decreasing = TRUE),c("feature","rank_Relieff")]
features_list$relieff = relieff_selected_features$feature[1:52]

for (j in c("mean_rank","mean_score","sum_scale")){
  for (i in c(5,10,15,20)){
  
    col_name = paste(j,i,sep = "_" )
    
    features = all_features[order(all_features[[j]], decreasing = TRUE),c("feature"),drop = F]
    features_list[[col_name]] = features$feature[1:i]
    
  }
  
}

for (i in names(features_list)){
  
  features = union(features_list[[i]] , demographics_features)
  combined = combined_bucket_amelia[,names(combined_bucket_amelia) %in% features] 
  
  cat("\n###########################################\n")
  cat(i)
  
  x_total = merge(Y_bucket, combined)
  
  y = x_total[, c(2:5)]
  x = x_total[,-c(1:5)]
  
  run_ridge(x,y[,2])
  run_tree_RF(x,y[,2])
  cat("\n###########################################\n")
}

