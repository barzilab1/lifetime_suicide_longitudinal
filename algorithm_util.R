library(doParallel)
library(caTools)
library(glmnet)
library(ROCR)
library(stir)
library(randomForest)

##########################################
# global variables of the below functions
##########################################
splits = 10000
N_CORES <- detectCores()
cat("\nno_cores " , N_CORES, "\n") 


# x: a dataframe with all the features not including bblid
run_rf_ridge = function(x,y,features_names){
  
  ridge_auc = list()
  rf_auc = list()
  
  cl = makeCluster(N_CORES, type="FORK")
  registerDoParallel(cl)
  
  set.seed(42)
  for (i in 1:splits) {
    
    #split the data splits times to 75% training and 25% test
    splitz = sample.split(y, .75)
    x_train <- x[splitz,]
    y_train <- y[splitz]
    x_test <- x[!splitz,]
    y_test <- y[!splitz]
    
    #' (1) imputation 
    if(imputation){
      
      x_train = missForest(x_train, parallelize = "forests")$ximp
      x_test = missForest(x_test, , parallelize = "forests")$ximp
      
    }else{
      #remove rows with NA
      index_train = which(rowSums(is.na(x_train)) == 0)
      x_train = x_train[index_train,]
      y_train = y_train[index_train]
      
      index_test = which(rowSums(is.na(x_test)) == 0)
      x_test = x_test[index_test,]
      y_test = y_test[index_test]
      
    }
    
    for(feature_set in names(features_names)){
      
      #get data according to bucket
      train_data = x_train[,features_names[[feature_set]]]
      test_data = x_test[,features_names[[feature_set]]]
      
      # cat("\nNA in train_data", which(is.na(train_data)), "colNumber", ncol(train_data), "rowNumber", nrow(train_data))
      # cat("\nNA in test_data", which(is.na(test_data)), "colNumber", ncol(test_data), "rowNumber", nrow(test_data), "\n")
      
      
      ###ridge
      mod <- cv.glmnet(x=as.matrix(train_data),y=y_train,alpha=0,family="binomial", parallel = T)
      opt_lambda <- mod$lambda.min
      mod <- mod$glmnet.fit
      y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(test_data),type ="response")
      pred <- prediction(y_predicted, y_test)
      
      ridge_auc[[feature_set]][i] =  performance(pred, measure = "auc")@y.values[[1]]
      
      ###rf
      mode_rf <- randomForest(x=train_data,y=as.factor(y_train))
      y_predicted <- predict(mode_rf, newdata = test_data, type ="prob")[,2]
      pred <- prediction(y_predicted, y_test)
      
      rf_auc[[feature_set]][i] =  performance(pred, measure = "auc")@y.values[[1]]
      
    }
    
  }
  
  stopCluster(cl)
  
  for(feature_set in names(ridge_auc)){
    
    cat("\n##################################\n")
    cat(feature_set)
    cat("\n##################################\n")
    
    cat("ridge auc: ",mean(ridge_auc[[feature_set]]))
    cat("\nrf auc: ",mean(rf_auc[[feature_set]]),"\n")
    
  }
  
  return(list("ridge_auc" = ridge_auc,"rf_auc" = rf_auc))

}

opt.cut.roc_ = function(fpr, sen,d){
  
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
opt.cut.roc = function(perf){
  
  d = (perf@x.values[[1]] - 0)^2 + (perf@y.values[[1]]-1)^2
  opt.cut.roc_(perf@x.values[[1]], perf@y.values[[1]],d)
}

#finds the closest point to 80% sen on the ROC curve
sen80.cut.roc = function(perf){
  
  d = abs((perf@y.values[[1]] - 0.8))
  opt.cut.roc_(perf@x.values[[1]], perf@y.values[[1]],d)
}

##########################################
# Lasso and Ridge
##########################################

# x: the features bucket
# y: the outcome variable
run_lasso <- function(x,y) {
  
  #initialize params 
  features <- matrix(0,nrow = ncol(x), ncol = 1)
  rownames(features) <- paste(colnames(x))
  
  lasso_measurements = matrix(NA,6,splits)
  rownames(lasso_measurements) = c("auc","sen","spe","acc","ppv","npv")
  
  lasso_80_sen = matrix(NA,splits,1)
  lasso_80_sen_spe = matrix(NA,splits,1)
  
  #enable parallelism
  cl = makeCluster(N_CORES, type="FORK")
  registerDoParallel(cl)
  
  set.seed(42)
  
  for (i in 1:splits) {
    
    #split the data splits times to 75% training and 25% test
    splitz = sample.split(y, .75)
    x_train <- x[splitz,]
    y_train <- y[splitz]
    x_test <- x[!splitz,]
    y_test <- y[!splitz]
    
    #imputation 
    if(imputation){
      
      X_train = missForest(x_train, parallelize = "forests")$ximp
      X_test = missForest(x_test, parallelize = "forests")$ximp
      
    }else{
      #remove rows with NA
      index_train = which(rowSums(is.na(x_train)) == 0)
      X_train = x_train[index_train,]
      y_train = y_train[index_train]
      
      index_test = which(rowSums(is.na(x_test)) == 0)
      X_test = x_test[index_test,]
      y_test = y_test[index_test]
      
    }
    
    #find best lambda
    mod <- cv.glmnet(x=as.matrix(X_train),y=y_train,alpha=1,family="binomial", parallel = TRUE)
    opt_lambda <- mod$lambda.min
    
    #get model
    mod <- mod$glmnet.fit
    
    #get selected features 
    coefs <- coef(mod, s=opt_lambda)
    #get all selected features not including the intercept 
    features[coefs@Dimnames[[1]][ which(coefs != 0 ) ][-1],1] = features[coefs@Dimnames[[1]][ which(coefs != 0 ) ][-1],1] +1
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(X_test),type ="response")
    pred <- prediction(y_predicted, y_test)
    
    #calculate AUC
    lasso_measurements["auc",i] <- performance(pred, measure = "auc")@y.values[[1]]
    
    #calculate sensitivity and specificity
    perf <- performance(pred,"tpr","fpr")
    
    #find closest point to (0,1)
    results = opt.cut.roc(perf)
    lasso_measurements["sen",i] = results["sensitivity"]
    lasso_measurements["spe",i] = results["specificity"]
    ind = results["ind"]
    
    #calculate acc
    acc = performance(pred, "acc" )
    lasso_measurements["acc", i] = acc@y.values[[1]][ind] 
    
    #calculate NPV and PPV
    perf_npv_ppv = performance(pred, "npv", "ppv" )
    lasso_measurements["npv",i] = perf_npv_ppv@y.values[[1]][ind] 
    lasso_measurements["ppv",i] = perf_npv_ppv@x.values[[1]][ind]  
    
    
    #find closest point to 80% sensitivity 
    results = sen80.cut.roc(perf)
    lasso_80_sen[i,1] = results["sensitivity"]
    lasso_80_sen_spe[i,1] = results["specificity"]
    
    #in case the edges were selected (sen=1, spe=0), replace them as always prefer tests with high spec
    if(lasso_80_sen[i,1] ==1 & lasso_80_sen_spe[i,1] ==0){
      lasso_80_sen[i,1] = 0 
      lasso_80_sen_spe[i,1] = 1
    }
    
  }
  
  
  stopCluster(cl)
    
  #calculate and print results
  cat("\n Lasso results \n")
  
  
  #get features
  cat("\nFeatures\n")
  features = features[order(features[,1], decreasing = TRUE),,drop = F]
  print(features)
  plot(features ,xlab="" ,ylab="Frequency", xaxt="n" , main="lasso", pch = 19)
  axis(1, at=1:nrow(features), labels=rownames(features))
  
  # avg. number of selected features. 
  cat("\nAvg. selected features")
  cat("\nall 10k models: " , sum(features)/splits, sep = "\t")  
  cat("\n")
  
  #get measurements closest to (0,1)
  cat("\nmeasurements: \n")
  print(apply(lasso_measurements, 1, mean, na.rm = T))
  cat("\nSD: \n")
  print(apply(lasso_measurements, 1, SD, na.rm = T))
  cat("\n")
  
  #get sen 80%
  cat("\nClosest point to Sensitivity = 80% ")
  cat("\nSensitivity", colMeans(lasso_80_sen, na.rm = T), sep="\t" ) 
  cat("\nSpecificity", colMeans(lasso_80_sen_spe, na.rm = T), sep="\t")
  cat("\n")
  
  return(list(features = features,
              number_selected = ceiling(sum(features)/splits),
              auc = mean(lasso_measurements["auc",])
              ))
}


#x: the features bucket
#y: the predicted variable
run_ridge <- function(x,y) {
  
  
  ridge_measurements = matrix(NA,6,splits)
  rownames(ridge_measurements) = c("auc","sen","spe","acc","ppv","npv")
  
  ridge_80_sen = matrix(NA,splits,1)
  ridge_80_sen_spe = matrix(NA,splits,1)
  
  cl = makeCluster(N_CORES, type="FORK")
  registerDoParallel(cl)
  
  set.seed(42)
  #split the data splits times to 75% training and 25% test
  for (i in 1:splits) {
    
    splitz = sample.split(y, .75)
    x_train <- x[splitz,]
    y_train <- y[splitz]
    x_test <- x[!splitz,]
    y_test <- y[!splitz]
    
    
    #find best lambda
    mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial", parallel = TRUE)
    opt_lambda <- mod$lambda.min
    
    #get model
    mod <- mod$glmnet.fit
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
    pred <- prediction(y_predicted, y_test)
    
    #calculate AUC
    ridge_measurements["auc",i] <- performance(pred, measure = "auc")@y.values[[1]]
    
    #calculate sensitivity and specificity
    perf <- performance(pred,"tpr","fpr")
    
    #find closest point to (0,1)
    results = opt.cut.roc(perf)
    ridge_measurements["sen",i] = results["sensitivity"]
    ridge_measurements["spe",i] = results["specificity"]
    ind = results["ind"]
    
    #calculate acc
    acc = performance(pred, "acc" )
    ridge_measurements["acc", i] = acc@y.values[[1]][ind] 
    
    #calculate NPV and PPV
    perf_npv_ppv = performance(pred, "npv", "ppv" )
    ridge_measurements["npv",i] = perf_npv_ppv@y.values[[1]][ind] 
    ridge_measurements["ppv",i] = perf_npv_ppv@x.values[[1]][ind] 
    
    
    #the 80% sensitivity 
    d = abs((perf@y.values[[1]] - 0.8))
    ind = which(d == min(d))
    if(length(ind)>1){
      #take the index with the biggest specificity 
      ind=ind[1]
    } 
    
    ridge_80_sen[i,1] = perf@y.values[[1]][ind]
    ridge_80_sen_spe[i,1] = 1 - perf@x.values[[1]][ind]
    
    #in case the edges were selected (sen=1, spe=0), replace them as always prefer tests with high spe
    if(ridge_80_sen[i,1] ==1 & ridge_80_sen_spe[i,1] == 0){
      ridge_80_sen[i,1] = 0 
      ridge_80_sen_spe[i,1] = 1
    }
    
  }
  
  
  stopCluster(cl)
  
  #calculate and print results
  cat("\n Ridge results \n")
  
  #get measurements closest to (0,1)
  cat("\nmeasurements: \n")
  print(apply(ridge_measurements, 1, mean, na.rm = T))
  cat("\nSD: \n")
  print(apply(ridge_measurements, 1, SD, na.rm = T))
  cat("\n")
  
  #get sen 80%
  cat("\nClosest point to Sensitivity = 80% ")
  cat("\nSensitivity", colMeans(ridge_80_sen, na.rm = T), sep = "\t" )
  cat("\nSpecificity", colMeans(ridge_80_sen_spe, na.rm = T), sep = "\t")
  cat("\n")
  
  ridge_measurements["auc",]
}


##########################################
# relieff (according to P_value)
##########################################

# x: the features bucket
# y: the outcome variable
run_stir <- function(x,y) {
  
  # create a parallel socket clusters
  cl = makeCluster(N_CORES, type="FORK")
  registerDoParallel(cl)
  
  results_list = foreach(i = seq(splits), .packages=c('stir','caTools') ) %dopar% {
   
      set.seed(i) # to keep iterations consistent
      
      #split the data splits times to 75% training and 25% test
      splitz = sample.split(y, .75)
      x_train <- x[splitz,]
      y_train <- y[splitz]
      
      #imputation 
      if(imputation){
        
        X_train = missForest(x_train, parallelize = "forests")$ximp
        
      }else{
        #remove rows with NA
        index_train = which(rowSums(is.na(x_train)) == 0)
        X_train = x_train[index_train,]
        y_train = y_train[index_train]
        
      }
      
      RF.method = "multisurf"
      metric <- "manhattan"
      neighbors <- find.neighbors(X_train, y_train, k = 0, method = RF.method)
      res <- stir(X_train, neighbors, k = 0, metric = metric, method = RF.method)$STIR_T
      res <- rownames(res[ (!is.na(res$t.pval) & res$t.pval < 0.05),])
      
    }
  
  stopCluster(cl)
  
  #get selected features
  res = table(unlist(results_list))
  features <- matrix(0,nrow = ncol(x), ncol = 1)
  rownames(features) <- paste(colnames(x))
  features[names(res),] =  data.frame(res,row.names = names(res))$Freq
  #how many were selected 
  tot_number = sum(res)
  
  
  cat("\n\nSelected Features According to Relieff\n")
  features = features[order(features, decreasing = TRUE),,drop=FALSE]
  print(features)
  plot(features ,xlab="" ,ylab="Frequency", xaxt="n" , main="stir", pch = 19)
  axis(1, at=1:nrow(features), labels=rownames(features))
  
  cat("\n Avg. selected features")
  cat("\nall 10k models: " , tot_number/splits, sep = "\t")  
  cat("\n")
  
  return(list(features = features,
              number_selected = ceiling(tot_number/splits)
              ))
  
}



##########################################
# Random Forest
##########################################

run_RF <- function(x,y) {
  
  features <- matrix(0,nrow = ncol(x), ncol = 2)
  rownames(features) <- paste(colnames(x))
  colnames(features) <- c("MeanDecreaseAccuracy","MeanDecreaseGini")
  
  rf_measurements = matrix(NA,6,splits)
  rownames(rf_measurements) = c("auc","sen","spe","acc","ppv","npv")
  
  set.seed(42)
  for (i in 1:splits) {
    
    splitz = sample.split(y, .75)
    x_train <- x[splitz,]
    y_train <- y[splitz]
    x_test <- x[!splitz,]
    y_test <- y[!splitz]
    
    #imputation 
    if(imputation){
      
      X_train = missForest(x_train, parallelize = "forests")$ximp
      X_test = missForest(x_test, parallelize = "forests")$ximp
      
    }else{
      #remove rows with NA
      index_train = which(rowSums(is.na(x_train)) == 0)
      X_train = x_train[index_train,]
      y_train = y_train[index_train]
      
      index_test = which(rowSums(is.na(x_test)) == 0)
      X_test = x_test[index_test,]
      y_test = y_test[index_test]
      
    }
    
    #RF
    mode_rf <- randomForest(x=X_train,y=as.factor(y_train), importance = TRUE)
    res <- mode_rf$importance
    features = features + res[match(rownames(features),rownames(res)),c("MeanDecreaseAccuracy","MeanDecreaseGini")]

    #calculate measurements
    y_predicted <- predict(mode_rf, newdata = X_test, type ="prob")[,2]
    pred <- prediction(y_predicted, y_test)
    
    #auc
    rf_measurements["auc",i] =  performance(pred, measure = "auc")@y.values[[1]]
    
    #calculate sensitivity and specificity
    perf <- performance(pred,"tpr","fpr")
    
    #find closest point to (0,1)
    results = opt.cut.roc(perf)
    rf_measurements["sen",i] = results["sensitivity"]
    rf_measurements["spe",i] = results["specificity"]
    ind = results["ind"]
    
    #calculate acc
    acc = performance(pred, "acc" )
    rf_measurements["acc", i] = acc@y.values[[1]][ind] 
    
    #calculate NPV and PPV
    perf_npv_ppv = performance(pred, "npv", "ppv" )
    rf_measurements["npv",i] = perf_npv_ppv@y.values[[1]][ind] 
    rf_measurements["ppv",i] = perf_npv_ppv@x.values[[1]][ind] 
    
  }
  
  cat("\n\nSelected Features According to Random Forest\n")
  features = features/splits
  
  
  #MeanDecreaseGini
  features_MeanDecreaseGini = features[order(features[,2], decreasing = TRUE),2,drop=FALSE]
  print(features_MeanDecreaseGini)
  plot(features_MeanDecreaseGini ,xlab="" ,ylab="", xaxt="n" , main="MeanDecreaseGini", pch = 19)
  axis(1, at=1:nrow(features), labels=rownames(features_MeanDecreaseGini))

  #get measurements closest to (0,1)
  cat("\nmeasurements: \n")
  print(apply(rf_measurements, 1, mean, na.rm = T))
  cat("\nSD: \n")
  print(apply(rf_measurements, 1, SD, na.rm = T))
  cat("\n")  
  
  return(list(features = features_MeanDecreaseGini,
              auc = rf_measurements["auc",]
        ))
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
  
  write.csv(all_features, "all_selected_features.csv", row.names = F)
  return(all_features)
}

find_biggest_gap = function(values){
  ind = 0
  diff = 0
  
  for (i in 1:(length(values)-1)){
    current_diff = (values[i] - values[i+1])
    if(current_diff > diff){
      diff = current_diff
      ind = i
    }
  }
  print(paste0("rf diff:", names(diff)))
  print(paste0("rf ind:", ind))
  ind
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
