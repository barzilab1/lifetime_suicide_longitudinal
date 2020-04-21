# bucket: a dataframe with all the features not including bblid
create_resids = function(bucket) {
  
  # make object to receive data. 
  resids <- matrix(NA,nrow(bucket),ncol(bucket))
  
  # loop through each column, predict it with all other variables, and take residuals. except bblid
  for (j in 1:ncol(bucket)) {
    mod <- lm(bucket[,j]~as.matrix(bucket[,-j]),data=bucket,na.action=na.exclude)
    resids[,j] <- scale(residuals(mod,na.action=na.exclude))
  }
  
  # append "res" to column names
  colnames(resids) <- paste(colnames(bucket),"_res",sep="")
  resids
}

get_logistic_results = function (model){
  data.frame(
      Estimate    = round(       summary(model)$coef[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05, "Estimate"] , digits = 3),
      EffectSize  = round(exp(   summary(model)$coef[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05, "Estimate"]), digits = 3),
      CI_2_5      = round(exp(confint.default(model)[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05,   1     ]  ), digits = 3),
      CI_97_5     = round(exp(confint.default(model)[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05,   2     ]  ), digits = 3),
      Pvalue      = round(       summary(model)$coef[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05, "Pr(>|z|)"] , digits = 3) 
  )
}

library(glmnet)
library(ROCR)
library(data.table)
library(caTools)
library(doParallel)

#finds the closest point to (0,1) on the ROC curve
opt.cut = function(perf, pred){
  
  temp = function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    # in case (0,0) (1,1) were selected, take spec = 1, sen =0 (0,0)  
    if (length(ind) == 2 & ind[1]==1 & ind[2]==length(x)){
      ind=ind[1]
    }else if(length(ind) > 1 & ind[length(ind)] != length(x)){
      #there are a few points with the same distance from (0,1), but it is not (0,0) (1,1)
      ind = ind[length(ind)]
    }else if(length(ind) > 1){
      ind=ind[1]
    }
    
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }
  
  
  cut.ind = mapply(temp, perf@x.values, perf@y.values, pred@cutoffs)
}


splits <- 10000
# Calculate the number of cores
number_cores <- detectCores()
cat("no_cores " , number_cores) 


# x: the features bucket
# y: the outcome variables bucket
# column_num: the index in y for which to calculate results. the order depends on y. assumption: 
# 1-Current_Suicidal_Ideation
# 2-Lifetime_Suicide_Attempt
# 3-Depression_mod_above_at_phq
run_lasso <- function(x,y, column_num) {
  
  lasso_auc <- matrix(NA,splits,3)
  colnames(lasso_auc) <- paste(colnames(y[,c(1:3)]))
  
  features <- matrix(0,nrow = ncol(x), ncol = 3)
  rownames(features) <- paste(colnames(x))
  colnames(features) <- paste(colnames(y[,c(1:3)]))
  
  number_of_features = matrix(0,2,3)
  colnames(number_of_features) = paste(colnames(y[,c(1:3)]))
  
  lasso_sen = matrix(NA,splits,3)
  colnames(lasso_sen) <- paste(colnames(y[,c(1:3)]))
  
  lasso_spe = matrix(NA,splits,3)
  colnames(lasso_spe) <- paste(colnames(y[,c(1:3)]))
  
  lasso_acc = matrix(NA,splits,3)
  colnames(lasso_acc) <- paste(colnames(y[,c(1:3)]))
  
  lasso_80_sen = matrix(NA,splits,3)
  colnames(lasso_80_sen) <- paste(colnames(y[,c(1:3)]))
  
  lasso_80_sen_spe = matrix(NA,splits,3)
  colnames(lasso_80_sen_spe) <- paste(colnames(y[,c(1:3)]))
  
  cl = makeCluster(number_cores, type="FORK")
  registerDoParallel(cl)
  
  #go over every y
  set.seed(42)
  # for (j in 1:3){
    j = column_num
    #split the data splits times to 75% training and 25% test
    for (i in 1:splits) {
      
      splitz = sample.split(y[[j]], .75)
      x_train <- x[splitz,]
      y_train <- y[splitz,j]
      x_test <- x[!(splitz),]
      y_test <- y[!(splitz),j]
      
      
      #find best lambda
      mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=1,family="binomial", parallel = TRUE)
      opt_lambda <- mod$lambda.min
      
      #get model
      mod <- mod$glmnet.fit
      
      #get selected features 
      coefs <- coef(mod, s=opt_lambda)
      #get all selected features not including the intercept 
      features[coefs@Dimnames[[1]][ which(coefs != 0 ) ][-1],j] = features[coefs@Dimnames[[1]][ which(coefs != 0 ) ][-1],j] +1
      #how many were selected 
      number_of_features[1,j] = number_of_features[1,j] + (length (which(coefs != 0 ) ) -1)
      #add one to the number of times more than 1 feature was selected 
      number_of_features[2,j] = ifelse( length(which(coefs != 0 ) ) > 1 , number_of_features[2,j] +1, number_of_features[2,j] )
      
      
      y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
      pred <- prediction(y_predicted, y_test)
      
      #calculate AUC
      lasso_auc[i,j] <- performance(pred, measure = "auc")@y.values[[1]]
      
      #calculate sensitivity and specificity
      perf <- performance(pred,"tpr","fpr")
      
      #find closest point to (0,1)
      results = opt.cut(perf, pred)
      lasso_sen[i,j] = results["sensitivity",1]
      lasso_spe[i,j] = results["specificity",1]
      
      #calculate acc
      acc = performance(pred, measure = "acc")
      ind = which(unlist(acc@x.values) == results["cutoff",1])
      lasso_acc[i,j] = acc@y.values[[1]][ind] 
      
      
      #the 80% sensitivity 
      dt = data.table(sen = perf@y.values[[1]], spe = 1-perf@x.values[[1]])
      setattr(dt, "sorted", "sen")
      index.sen = dt[.(0.8), roll = "nearest", which = TRUE]
      if (length(index.sen) > 1 ){
        #take the index with the biggest specificity 
        index.sen = index.sen[1]
      }
      lasso_80_sen[i,j] = dt$sen[index.sen]
      lasso_80_sen_spe[i,j] = dt$spe[index.sen]
      
      #in case the edges were selected (sen=1, spe=0), replace them as alwayes prefer tests with high spec
      if(lasso_80_sen[i,j] ==1 & lasso_80_sen_spe[i,j] ==0){
        lasso_80_sen[i,j] = 0 
        lasso_80_sen_spe[i,j] = 1
      }
      
    }
  # }
  
  stopCluster(cl)
    
  #calculate and print results
  cat("\n Lasso results for", colnames(lasso_auc)[column_num],"\n")
  
  #calculate mean AUC
  cat("\nAUC\n")
  print(apply(lasso_auc, 2, mean, na.rm=TRUE))
  cat("\n")
  
  #get features
  cat("\nFeatures\n")
  features = features[order(features[,column_num], decreasing = TRUE),]
  print(features)
  plot(features[,column_num] ,xlab="" ,ylab="Frequency", xaxt="n" , main=colnames(features)[column_num], pch = 19)
  axis(1, at=1:nrow(features), labels=rownames(features))
  
  # avg. number of selected features. 
  cat("\n Avg. selected features")
  cat("\n all 100 models: " , number_of_features[1,column_num]/splits, sep = "\t")  
  cat("\n only models that selected features: " , number_of_features[1,column_num]/number_of_features[2,column_num], sep = "\t")  
  cat("\n")
  
  #get sen, spe and acc closest to (0,1)
  cat("\n Closest point on the ROC to (0,1) ")
  cat("\n Sensitivity \n")
  print(apply(lasso_sen, 2, mean, na.rm=TRUE))
  cat("\n Specificity \n")
  print(apply(lasso_spe, 2, mean, na.rm=TRUE))
  cat("\n Accurecy \n")
  print(apply(lasso_acc, 2, mean, na.rm=TRUE))
  cat("\n")
  
  
  #get sen 80%
  cat("\n Closest point to Sensitivity = 80% ")
  cat("\n Sensitivity \n")
  print(apply(lasso_80_sen, 2, mean, na.rm=TRUE))
  cat("\n Specificity \n")
  print(apply(lasso_80_sen_spe, 2, mean, na.rm=TRUE))
}


#x: the features bucket
#y: the predicted values bucket
run_ridge <- function(x,y,column_num) {
  
  ridge_auc <- matrix(NA,splits,3)
  colnames(ridge_auc) <- paste(colnames(y[,c(1:3)]))
  
  ridge_sen = matrix(NA,splits,3)
  colnames(ridge_sen) <- paste(colnames(y[,c(1:3)]))
  
  ridge_spe = matrix(NA,splits,3)
  colnames(ridge_spe) <- paste(colnames(y[,c(1:3)]))
  
  ridge_acc = matrix(NA,splits,3)
  colnames(ridge_acc) <- paste(colnames(y[,c(1:3)]))
  
  ridge_80_sen = matrix(NA,splits,3)
  colnames(ridge_80_sen) <- paste(colnames(y[,c(1:3)]))
  
  ridge_80_sen_spe = matrix(NA,splits,3)
  colnames(ridge_80_sen_spe) <- paste(colnames(y[,c(1:3)]))
  
  cl = makeCluster(number_cores, type="FORK")
  registerDoParallel(cl)
  
  #go over every y
  set.seed(42)
  # for (j in 1:3){
    j = column_num
    #split the data splits times to 75% training and 25% test
    for (i in 1:splits) {
      
      splitz = sample.split(y[[j]], .75)
      x_train <- x[splitz,]
      y_train <- y[splitz,j]
      x_test <- x[!(splitz),]
      y_test <- y[!(splitz),j]
      
      
      #find best lambda
      mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial", parallel = TRUE)
      opt_lambda <- mod$lambda.min
      
      #get model
      mod <- mod$glmnet.fit
      
      y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
      pred <- prediction(y_predicted, y_test)
      
      #calculate AUC
      ridge_auc[i,j] <- performance(pred, measure = "auc")@y.values[[1]]
      
      #calculate sensitivity and specificity
      perf <- performance(pred,"tpr","fpr")
      
      #find closest point to (0,1)
      results = opt.cut(perf, pred)
      ridge_sen[i,j] = results["sensitivity",1]
      ridge_spe[i,j] = results["specificity",1]
      
      #calculate acc
      acc = performance(pred, measure = "acc")
      ind = which(unlist(acc@x.values) == results["cutoff",1])
      ridge_acc[i,j] = acc@y.values[[1]][ind] 
      
      
      #the 80% sensitivity 
      dt = data.table(sen = perf@y.values[[1]], spe = 1-perf@x.values[[1]])
      setattr(dt, "sorted", "sen")
      index.sen = dt[.(0.8), roll = "nearest", which = TRUE]
      if (length(index.sen) > 1 ){
        #take the index with the biggest specificity 
        index.sen = index.sen[1]
      }
      ridge_80_sen[i,j] = dt$sen[index.sen]
      ridge_80_sen_spe[i,j] = dt$spe[index.sen]
      
      #in case the edges were selected (sen=1, spe=0), replace them as alwayes prefer tests with high spe
      if(ridge_80_sen[i,j] ==1 & ridge_80_sen_spe[i,j] == 0){
        ridge_80_sen[i,j] = 0 
        ridge_80_sen_spe[i,j] = 1
      }
      
    }
  # }
  
  stopCluster(cl)
  
  #calculate and print results
  cat("\n Ridge results \n")
  
  #calculate mean AUC
  cat("\nAUC\n")
  print(apply(ridge_auc, 2, mean, na.rm=TRUE))
  cat("\n")
  
  #get sen, spe and acc closest to (0,1)
  cat("\n Closest point on the ROC to (0,1) ")
  cat("\n Sensitivity \n")
  print(apply(ridge_sen, 2, mean, na.rm=TRUE))
  cat("\n Specificity \n")
  print(apply(ridge_spe, 2, mean, na.rm=TRUE))
  cat("\n Accurecy \n")
  print(apply(ridge_acc, 2, mean, na.rm=TRUE))
  cat("\n")
  
  #get sen 80%
  cat("\n Closest point to Sensitivity = 80% ")
  cat("\n Sensitivity \n")
  print(apply(ridge_80_sen, 2, mean, na.rm=TRUE))
  cat("\n Specificity \n")
  print(apply(ridge_80_sen_spe, 2, mean, na.rm=TRUE))
}


##########################################
# relieff (according to P_value)
##########################################
# library(stir)


# x: the features bucket
# y: the outcome variables bucket
# column_num: the index in y for which to calculate results. the order depends on y. assumption: 
# 1-Current_Suicidal_Ideation
# 2-Lifetime_Suicide_Attempt
# 3-Depression_mod_above_at_phq
run_stir <- function(x,y,column_num) {
  
  # create a parallel socket clusters
  cl = makeCluster(number_cores, type="FORK")
  registerDoParallel(cl)
  
  features <- matrix(0,nrow = ncol(x), ncol = 1)
  rownames(features) <- paste(colnames(x))
  
  results_list = foreach(i = seq(splits), .packages=c('stir','caTools') ) %dopar% {
   
      set.seed(i) # to keep iterations consistent
      
      #split the data splits times to 75% training and 25% test
      splitz = sample.split(y[[column_num]], .75)
      x_train <- x[splitz,]
      y_train <- y[splitz,column_num]
      
      RF.method = "multisurf"
      metric <- "manhattan"
      neighbors <- find.neighbors(x_train, y_train, k = 0, method = RF.method)
      res <- stir(x_train, neighbors, k = 0, metric = metric, method = RF.method)$STIR_T
      res <- rownames(res[ (!is.na(res$t.pval) & res$t.pval < 0.05),])
      
    }
  
  stopCluster(cl)
  
  #get selected features
  res = table(unlist(results_list))
  features[names(res),] =  data.frame(res,row.names = names(res))$Freq
  #how many were selected 
  tot_number = sum(sapply(results_list, length))
  #add one to the number of times more than 1 feature was selected 
  tot_selected_model = sum(sapply(results_list, function(x){ length(x)>0}))
  
  
  cat("\n\nSelected Features According to Relieff\n")
  features = features[order(features[,1], decreasing = TRUE),,drop=FALSE]
  print(features)
  plot(features[,1] ,xlab="" ,ylab="Frequency", xaxt="n" , main="stir", pch = 19)
  axis(1, at=1:nrow(features), labels=rownames(features))
  
  cat("\n Avg. selected features")
  cat("\n all 10k models: " , tot_number/splits, sep = "\t")  
  cat("\n only models that selected features: " , tot_number/tot_selected_model, sep = "\t")  
  cat("\n")
  
}



##########################################
# Random Forest and Decision Trees
##########################################

library(randomForest)
library(tree)

run_tree_RF <- function(x,y) {
  
  features <- matrix(0,nrow = ncol(x), ncol = 2)
  rownames(features) <- paste(colnames(x))
  colnames(features) <- c("MeanDecreaseAccuracy","MeanDecreaseGini")
  
  features_tree <- matrix(0,nrow = ncol(x), ncol = 1)
  rownames(features_tree) <- paste(colnames(x))
  
  rf_auc = 0
  tree_auc = 0
  
  number_of_features = c(0,0)
  
  set.seed(42)
  for (i in 1:splits) {
    
    splitz = sample.split(y, .75)
    x_train <- x[splitz,]
    y_train <- y[splitz]
    x_test <- x[!splitz,]
    y_test <- y[!splitz]

    #RF
    mod1 <- randomForest(x=x_train,y=as.factor(y_train), importance = TRUE)
    res <- mod1$importance
    index = ncol(res) -1
    features = features + res[match(rownames(features),rownames(res)),index:(index+1)]
    
    #calculate AUC
    y_predicted <- predict(mod1, newdata = x_test, type ="prob")[,2]
    pred <- prediction(y_predicted, y_test)
    rf_auc = rf_auc + performance(pred, measure = "auc")@y.values[[1]]
    
    # tree
    data_= cbind(y_train,x_train)
    mode_tree = tree(as.factor(y_train) ~ ., data_)
    used_features = summary(mode_tree)$used
    features_tree[as.character(used_features),1] = features_tree[as.character(used_features),1] + 1 
    #how many were selected 
    number_of_features[1] = number_of_features[1] + length(used_features) 
    #add one to the number of times more than 1 feature was selected 
    number_of_features[2] = ifelse( length(used_features) > 0 , number_of_features[2] +1, number_of_features[2] )
    
    #calculate AUC
    y_predicted <- predict(mode_tree, newdata = x_test)[,2]
    pred <- prediction(y_predicted, y_test)
    tree_auc = tree_auc + performance(pred, measure = "auc")@y.values[[1]]
    
  }
  
  cat("\n\nSelected Features According to Random Forest\n")
  features = features/splits
  
  #MeanDecreaseAccuracy
  # features_MeanDecreaseAccuracy = features[order(features[,1], decreasing = TRUE),1,drop=FALSE]
  # print(features_MeanDecreaseAccuracy)
  # plot(features_MeanDecreaseAccuracy ,xlab="" ,ylab="", xaxt="n" , main="MeanDecreaseAccuracy", pch = 19)
  # axis(1, at=1:nrow(features), labels=rownames(features_MeanDecreaseAccuracy))
  # cat("\n")
  
  #MeanDecreaseGini
  features_MeanDecreaseGini = features[order(features[,2], decreasing = TRUE),2,drop=FALSE]
  print(features_MeanDecreaseGini)
  plot(features_MeanDecreaseGini ,xlab="" ,ylab="", xaxt="n" , main="MeanDecreaseGini", pch = 19)
  axis(1, at=1:nrow(features), labels=rownames(features_MeanDecreaseGini))
  cat("\n RF AUC: ", rf_auc/splits, sep = "\t")
  
  
  cat("\n\nSelected Features According to Decision Trees \n")
  features_tree = features_tree[order(features_tree[,1], decreasing = TRUE),1,drop=FALSE]
  print(features_tree)
  plot(features_tree ,xlab="" ,ylab="", xaxt="n" , main="tree features", pch = 19)
  axis(1, at=1:nrow(features_tree), labels=rownames(features_tree))
  
  # avg. number of selected features. 
  cat("\n Avg. selected features")
  cat("\n all models: " , number_of_features[1]/splits, sep = "\t")  
  cat("\n only models that selected features: " , number_of_features[1]/number_of_features[2], sep = "\t")  
  cat("\n")
  cat("\n tree AUC: ", tree_auc/splits, sep = "\t")
}


