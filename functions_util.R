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
opt.cut.roc = function(perf, pred){
  
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
    
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]], ind = ind)
  }
  
  
  cut.ind = mapply(temp, perf@x.values, perf@y.values, pred@cutoffs)
}


# global variables of the below functions
splits <- 10000
# Calculate the number of cores
number_cores <- detectCores()
cat("\nno_cores " , number_cores, "\n") 


# x: the features bucket
# y: the outcome variable
run_lasso <- function(x,y) {
  
  
  features <- matrix(0,nrow = ncol(x), ncol = 1)
  rownames(features) <- paste(colnames(x))
  
  number_of_features = c(0,0)
  
  lasso_measurements = matrix(NA,6,splits)
  rownames(lasso_measurements) = c("auc","sen","spe","acc","ppv","npv")
  
  lasso_80_sen = matrix(NA,splits,1)
  lasso_80_sen_spe = matrix(NA,splits,1)
  
  cl = makeCluster(number_cores, type="FORK")
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
    mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=1,family="binomial", parallel = TRUE)
    opt_lambda <- mod$lambda.min
    
    #get model
    mod <- mod$glmnet.fit
    
    #get selected features 
    coefs <- coef(mod, s=opt_lambda)
    #get all selected features not including the intercept 
    features[coefs@Dimnames[[1]][ which(coefs != 0 ) ][-1],1] = features[coefs@Dimnames[[1]][ which(coefs != 0 ) ][-1],1] +1
    #how many were selected 
    number_of_features[1] = number_of_features[1] + (length (which(coefs != 0 ) ) -1)
    #add one to the number of times more than 1 feature was selected 
    number_of_features[2] = ifelse( length(which(coefs != 0 ) ) > 1 , number_of_features[2] +1, number_of_features[2] )
    
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
    pred <- prediction(y_predicted, y_test)
    
    #calculate AUC
    lasso_measurements["auc",i] <- performance(pred, measure = "auc")@y.values[[1]]
    
    #calculate sensitivity and specificity
    perf <- performance(pred,"tpr","fpr")
    
    #find closest point to (0,1)
    results = opt.cut.roc(perf, pred)
    lasso_measurements["sen",i] = results["sensitivity",1]
    lasso_measurements["spe",i] = results["specificity",1]
    ind = results["ind",1]
    
    #calculate acc
    acc = performance(pred, "acc" )
    lasso_measurements["acc", i] = acc@y.values[[1]][ind] 
    
    #calculate NPV and PPV
    perf_npv_ppv = performance(pred, "npv", "ppv" )
    lasso_measurements["npv",i] = perf_npv_ppv@y.values[[1]][ind] 
    lasso_measurements["ppv",i] = perf_npv_ppv@x.values[[1]][ind]  
    
    
    #the 80% sensitivity 
    dt = data.table(sen = perf@y.values[[1]], spe = 1-perf@x.values[[1]])
    setattr(dt, "sorted", "sen")
    index.sen = dt[.(0.8), roll = "nearest", which = TRUE]
    if (length(index.sen) > 1 ){
      #take the index with the biggest specificity 
      index.sen = index.sen[1]
    }
    lasso_80_sen[i,1] = dt$sen[index.sen]
    lasso_80_sen_spe[i,1] = dt$spe[index.sen]
    
    #in case the edges were selected (sen=1, spe=0), replace them as alwayes prefer tests with high spec
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
  cat("\nall 10k models: " , number_of_features[1]/splits, sep = "\t")  
  cat("\nonly models that selected features: " , number_of_features[1]/number_of_features[2], sep = "\t")  
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
  
  return(features)
}


#x: the features bucket
#y: the predicted variable
run_ridge <- function(x,y) {
  
  
  ridge_measurements = matrix(NA,6,splits)
  rownames(ridge_measurements) = c("auc","sen","spe","acc","ppv","npv")
  
  ridge_80_sen = matrix(NA,splits,1)
  ridge_80_sen_spe = matrix(NA,splits,1)
  
  cl = makeCluster(number_cores, type="FORK")
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
    results = opt.cut.roc(perf, pred)
    ridge_measurements["sen",i] = results["sensitivity",1]
    ridge_measurements["spe",i] = results["specificity",1]
    ind = results["ind",1]
    
    #calculate acc
    acc = performance(pred, "acc" )
    ridge_measurements["acc", i] = acc@y.values[[1]][ind] 
    
    #calculate NPV and PPV
    perf_npv_ppv = performance(pred, "npv", "ppv" )
    ridge_measurements["npv",i] = perf_npv_ppv@y.values[[1]][ind] 
    ridge_measurements["ppv",i] = perf_npv_ppv@x.values[[1]][ind] 
    
    
    #the 80% sensitivity 
    dt = data.table(sen = perf@y.values[[1]], spe = 1-perf@x.values[[1]])
    setattr(dt, "sorted", "sen")
    index.sen = dt[.(0.8), roll = "nearest", which = TRUE]
    if (length(index.sen) > 1 ){
      #take the index with the biggest specificity 
      index.sen = index.sen[1]
    }
    ridge_80_sen[i,1] = dt$sen[index.sen]
    ridge_80_sen_spe[i,1] = dt$spe[index.sen]
    
    #in case the edges were selected (sen=1, spe=0), replace them as alwayes prefer tests with high spe
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
}


##########################################
# relieff (according to P_value)
##########################################
# library(stir)


# x: the features bucket
# y: the outcome variable
run_stir <- function(x,y) {
  
  # create a parallel socket clusters
  cl = makeCluster(number_cores, type="FORK")
  registerDoParallel(cl)
  
  features <- matrix(0,nrow = ncol(x), ncol = 1)
  rownames(features) <- paste(colnames(x))
  
  results_list = foreach(i = seq(splits), .packages=c('stir','caTools') ) %dopar% {
   
      set.seed(i) # to keep iterations consistent
      
      #split the data splits times to 75% training and 25% test
      splitz = sample.split(y, .75)
      x_train <- x[splitz,]
      y_train <- y[splitz]
      
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
  cat("\nall 10k models: " , tot_number/splits, sep = "\t")  
  cat("\nonly models that selected features: " , tot_number/tot_selected_model, sep = "\t")  
  cat("\n")
  
  return(features)
  
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
  
  rf_measurements = matrix(NA,6,splits)
  rownames(rf_measurements) = c("auc","sen","spe","acc","ppv","npv")
  
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
    mode_rf <- randomForest(x=x_train,y=as.factor(y_train), importance = TRUE)
    res <- mode_rf$importance
    index = ncol(res) -1
    features = features + res[match(rownames(features),rownames(res)),index:(index+1)]
    
    #calculate measurements
    y_predicted <- predict(mode_rf, newdata = x_test, type ="prob")[,2]
    pred <- prediction(y_predicted, y_test)
    
    #auc
    rf_measurements["auc",i] =  performance(pred, measure = "auc")@y.values[[1]]
    
    #calculate sensitivity and specificity
    perf <- performance(pred,"tpr","fpr")
    
    #find closest point to (0,1)
    results = opt.cut.roc(perf, pred)
    rf_measurements["sen",i] = results["sensitivity",1]
    rf_measurements["spe",i] = results["specificity",1]
    ind = results["ind",1]
    
    #calculate acc
    acc = performance(pred, "acc" )
    rf_measurements["acc", i] = acc@y.values[[1]][ind] 
    
    #calculate NPV and PPV
    perf_npv_ppv = performance(pred, "npv", "ppv" )
    rf_measurements["npv",i] = perf_npv_ppv@y.values[[1]][ind] 
    rf_measurements["ppv",i] = perf_npv_ppv@x.values[[1]][ind] 
    
    # tree
    # data_= cbind(y_train,x_train)
    # mode_tree = tree(as.factor(y_train) ~ ., data_)
    # used_features = summary(mode_tree)$used
    # features_tree[as.character(used_features),1] = features_tree[as.character(used_features),1] + 1 
    # #how many were selected 
    # number_of_features[1] = number_of_features[1] + length(used_features) 
    # #add one to the number of times more than 1 feature was selected 
    # number_of_features[2] = ifelse( length(used_features) > 0 , number_of_features[2] +1, number_of_features[2] )
    # 
    # #calculate AUC
    # y_predicted <- predict(mode_tree, newdata = x_test)[,2]
    # pred <- prediction(y_predicted, y_test)
    # tree_auc = tree_auc + performance(pred, measure = "auc")@y.values[[1]]
    
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

  #get measurements closest to (0,1)
  cat("\nmeasurements: \n")
  print(apply(rf_measurements, 1, mean, na.rm = T))
  cat("\nSD: \n")
  print(apply(rf_measurements, 1, SD, na.rm = T))
  cat("\n")  
  
  # cat("\n\nSelected Features According to Decision Trees \n")
  # features_tree = features_tree[order(features_tree[,1], decreasing = TRUE),1,drop=FALSE]
  # print(features_tree)
  # plot(features_tree ,xlab="" ,ylab="", xaxt="n" , main="tree features", pch = 19)
  # axis(1, at=1:nrow(features_tree), labels=rownames(features_tree))
  # 
  # # avg. number of selected features. 
  # cat("\n Avg. selected features")
  # cat("\n all models: " , number_of_features[1]/splits, sep = "\t")  
  # cat("\n only models that selected features: " , number_of_features[1]/number_of_features[2], sep = "\t")  
  # cat("\n")
  # cat("\n tree AUC: ", tree_auc/splits, sep = "\t")
  # cat("\n")
  
  return(features_MeanDecreaseGini)
}


# y: the main variable to check significance with 
#data:  a matrix of feature to compre with
check_significance = function(y,data){
  
  results = matrix(F, nrow = 1 ,ncol = length(data))
  colnames(results) = names(data)
  
  for (i in 1:length(data)) {
    difference = as.matrix(y - data[,i])
    hist(difference, main = names(data)[i])
    interval = mean(difference) + c(-2,2)*sd(difference)
    results[i] = ifelse( interval[1] <= 0 & 0 <= interval[2], F, T)
  }
  print(results)
}
