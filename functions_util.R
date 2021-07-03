library(caTools)

find_opt_cut_roc_ = function(fpr, sen,d){
  
  ind = which(d == min(d))
  # in case (0,0) (1,1) were selected, take spec = 1, sen =0 (0,0)  
  if (length(ind) == 2 & ind[2]==length(fpr)){
    ind=ind[1]
    # }else if(length(ind) > 1 & ind[length(ind)] != length(fpr)){
    #   #there are a few points with the same distance from (0,1), but it is not (0,0) (1,1)
    #   ind = ind[length(ind)]
  }else if(length(ind) > 1){
    ind=ind[1]
  }
  
  c(sensitivity = sen[[ind]], specificity = 1-fpr[[ind]], ind = ind)
}


#finds the closest point to (0,1) on the ROC curve
find_opt_cut_roc = function(perf){
  
  d = (perf@x.values[[1]] - 0)^2 + (perf@y.values[[1]]-1)^2
  find_opt_cut_roc_(perf@x.values[[1]], perf@y.values[[1]],d)
}


#finds the closest point to 80% sen on the ROC curve
find_sen80_cut_roc = function(perf){
  
  d = abs((perf@y.values[[1]] - 0.8))
  find_opt_cut_roc_(perf@x.values[[1]], perf@y.values[[1]],d)
}




rank_features = function(res_lasso,res_Relieff,res_rf){
  res_lasso_updated = data.frame(score_lasso = res_lasso$features, rank_lasso = rank(res_lasso$features, ties.method= "max"))
  res_lasso_updated$feature = rownames(res_lasso_updated)
  res_lasso_updated[res_lasso_updated$score_lasso == 0, "rank_lasso"] = 0
  
  res_Relieff_updated = data.frame(score_Relieff = res_Relieff$features, rank_Relieff = rank(res_Relieff$features, ties.method= "max"))
  res_Relieff_updated$feature = rownames(res_Relieff_updated)
  res_Relieff_updated[res_Relieff_updated$score_Relieff == 0, "rank_Relieff"] = 0
  
  res_rf_updated = data.frame(score_rf = res_rf$features[,1], rank_rf = rank(res_rf$features[,1], ties.method= "max"))
  res_rf_updated$feature = rownames(res_rf_updated)
  
  all_features = merge(res_lasso_updated,res_Relieff_updated)
  all_features = merge(all_features,res_rf_updated)
  
  all_features$mean_rank = round(rowMeans(all_features[,c("rank_lasso", "rank_Relieff", "rank_rf")]), digits = 3)
  
  write.csv(all_features, "output/all_selected_features.csv", row.names = F)
  return(all_features)
}


find_biggest_gap = function(values){
  
  ordered_values = values[order(values,decreasing = T)]
  
  ind = 0
  diff = 0
  
  for (i in 1:(length(ordered_values)-1)){
    current_diff = (ordered_values[i] - ordered_values[i+1])
    if(current_diff > diff){
      diff = current_diff
      ind = names(ordered_values)[i]
    }
  }
  
  print(paste0("rf diff:", current_diff))
  print(paste0("rf ind:", ind))
  print(paste0("rf value:", ordered_values[ind]))
  
  return(ordered_values[ind])
}

# y: the main variable to check significance with 
#data:  a matrix of feature to compare with
check_significance = function(y,data,main_text){
  
  results = matrix(F, nrow = 1 ,ncol = length(data))
  colnames(results) = names(data)
  
  for (i in 1:length(data)) {
    difference = as.matrix(y - data[,i])
    # hist(difference, main = paste0(names(data)[i],"_" ,main_text))
    interval = mean(difference) + c(-2,2)*sd(difference)
    results[i] = ifelse( interval[1] <= 0 & 0 <= interval[2], F, T)
  }
  print(results)
}

calculate_measurments = function(pred) {
  measurements = c()
  #auc
  measurements["auc"] = performance(pred, measure = "auc")@y.values[[1]]
  
  #calculate sensitivity and specificity
  perf <- performance(pred,"tpr","fpr")
  
  #find closest point to (0,1)
  results = find_opt_cut_roc(perf)
  measurements["sen"] = results["sensitivity"]
  measurements["spe"] = results["specificity"]
  ind = results["ind"]
  
  #calculate NPV and PPV
  perf_npv_ppv = performance(pred, "npv", "ppv" )
  # measurements["ppv"] = perf_npv_ppv@x.values[[1]][ind]
  measurements["npv"] = perf_npv_ppv@y.values[[1]][ind] 
  measurements["ppv"] = perf_npv_ppv@x.values[[1]][ind]
  
  
  return (measurements)
}

