library(openxlsx)

# global variables of the below functions
N_FOLDS = 10



# x: all features 
# y: the outcome variable
run_algos <- function(x,y) {
  
  set.seed(42)
  
  #create folders of y
  folds = createFolds(y, k = N_FOLDS)
  
  ridge_measurements = list()
  rf_measurements = list()
  ridge_measurements$lasso = list()
  rf_measurements$lasso = list()
  ridge_measurements$relieff = list()
  rf_measurements$relieff = list()
  
  for (j in c("mean_rank","rank_rf")){
    for (i in c(5,10,13,14,15,20,25,30,35,40,45)){
      
      col_name = paste(j,i,sep = "_" )
      ridge_measurements[[col_name]] = list()
      rf_measurements[[col_name]] = list()
      
    }
  }
  
  demographics_features = c("bblid","sex","age","race2_White","race2_Black","goassessPhqDurMonths")
  
  
  for (f_index in 1:N_FOLDS) {
    
    cat("\n################",f_index,"############################\n")
    
    fold = folds[[f_index]]
    X_train = x[-fold, ]
    X_test = x[fold, ]
    
    y_train = y[-fold]  
    y_test = y.test.orig = y[fold]
    
    #' (1) imputation 
    if(imputation){
      
      X_train = missForest(X_train)$ximp
      X_test = missForest(X_test)$ximp
      
      cat("\nNA in train\n", which(is.na(X_train), arr.ind=TRUE))
      cat("NA in test\n", which(is.na(X_test), arr.ind=TRUE))

      write.csv(summary(X_test), paste("res/X_test",f_index,".csv",sep = "_" ))
      write.csv(summary(X_train), paste("res/X_train",f_index,".csv",sep = "_" ))
      
    }else{
      #remove rows with NA
      index_train = which(rowSums(is.na(X_train)) == 0)
      X_train = X_train[index_train,]
      y_train = y_train[index_train]
      
      index_test = which(rowSums(is.na(X_test)) == 0)
      X_test = X_test[index_test,]
      y_test = y_test[index_test]
      
      cat("\nnumber of train rows: ", nrow(X_train))
      cat("\nnumber of test rows: ", nrow(X_test))
    }
    
    #' (2) run feature selection on training: lasso, RF, relieff 
    res_lasso = run_lasso(X_train,y_train)
    res_Relieff = run_stir(X_train,y_train)
    res_rf = run_tree_RF(X_train,y_train)
    
    #' (3) get ranked lists and mean them to get list #4
    lasso_ranked_features = data.frame(score_lasso = res_lasso$features, rank_lasso = rank(res_lasso$features, ties.method= "max"))
    lasso_ranked_features$feature = rownames(res_lasso$features)
    lasso_ranked_features[lasso_ranked_features$score_lasso == 0, "rank_lasso"] = 0
    relieff_ranked_features = data.frame(score_Relieff = res_Relieff$features, rank_Relieff = rank(res_Relieff$features, ties.method= "max"))
    relieff_ranked_features$feature = rownames(res_Relieff$features)
    relieff_ranked_features[relieff_ranked_features$score_Relieff == 0, "rank_Relieff"] = 0
    rf_ranked_features = data.frame(score_rf = res_rf$features, rank_rf = rank(res_rf$features, ties.method= "max"))
    rf_ranked_features$feature = rownames(res_rf$features)
    
    all_features = merge(lasso_ranked_features,relieff_ranked_features)
    all_features = merge(all_features,rf_ranked_features)
    all_features$mean_rank = round(rowMeans(all_features[,c("rank_lasso", "rank_Relieff", "rank_rf")]), digits = 3)
    
    
    #' (4) print the results
    write.csv(all_features, paste("res/all_features_ranked_",f_index,".csv",sep = "" ), row.names = F)
    
    
    #' (5) get 4 seb sets 
    features_list = list()
    features_list$lasso = all_features[order(all_features$rank_lasso, decreasing = TRUE),"feature"][1:res_lasso$number_selected]
    features_list$relieff  = all_features[order(all_features$rank_Relieff, decreasing = TRUE),"feature"][1:res_Relieff$number_selected]
    
    for (j in c("mean_rank","rank_rf")){
      for (i in c(5,10,13,14,15,20,25,30,35,40,45)){
        
        col_name = paste(j,i,sep = "_" )
        features_list[[col_name]] = all_features[order(all_features[[j]], decreasing = TRUE),"feature"][1:i]
        
      }
    }
    
    
    #' (6) for each subst:
    #' - train rf/ridge on training dataset
    #' - calculate all measurements for testing dataset
    #' - check statistically difference? 
    #' outputs: 
    #' for each fold, the mesurmentsx2
    
    cl = makeCluster(N_CORES, type="FORK")
    registerDoParallel(cl)
    for (j in names(features_list)){
      
      cat("\n################",j,"############################\n")
      
      #get data
      train_data = X_train[,names(X_train) %in% c(features_list[[j]], demographics_features)] 
      test_data = X_test[,names(X_test) %in% c(features_list[[j]], demographics_features)] 
      
      cat("\nNA in train_data", which(is.na(train_data)), "colNumber", ncol(train_data), "rowNumber", nrow(train_data))
      cat("\nNA in test_data", which(is.na(test_data)), "colNumber", ncol(test_data), "rowNumber", nrow(test_data), "\n")
      
      
      ###ridge
      mod <- cv.glmnet(x=as.matrix(train_data),y=y_train,alpha=0,family="binomial", parallel = T)
      opt_lambda <- mod$lambda.min
      mod <- mod$glmnet.fit
      y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(test_data),type ="response")
      pred <- prediction(y_predicted, y_test)
      
      ridge_measurements[[j]][[f_index]] = calculate_measurments(pred)
      
      ###rf
      mode_rf <- randomForest(x=train_data,y=as.factor(y_train))
      y_predicted <- predict(mode_rf, newdata = test_data, type ="prob")[,2]
      pred <- prediction(y_predicted, y_test)
      
      rf_measurements[[j]][[f_index]] = calculate_measurments(pred)
      
      
    }
    
    stopCluster(cl)
    
  }

  
  wb = createWorkbook()
  
  for(j in names(rf_measurements)){
    
    sheet_name_ridge = paste("ridge",j ,sep = "__" )
    sheet_name_rf = paste("rf",j ,sep = "__" )
    
    addWorksheet(wb, sheet_name_ridge)
    addWorksheet(wb, sheet_name_rf)
    
    writeData(wb, sheet_name_ridge, as.data.frame(ridge_measurements[[j]]), rowNames = T)
    writeData(wb, sheet_name_rf, as.data.frame(rf_measurements[[j]]), rowNames = T)
  }
  
  saveWorkbook(wb, "res/results.xlsx", overwrite = TRUE)
  
   
}