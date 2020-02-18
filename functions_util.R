# bucket: a df with all the features not including bblid
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
      Estimate    =      summary(model)$coef[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05, "Estimate"]  ,
      EffectSize  = exp( summary(model)$coef[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05, "Estimate"] ),
      CI_2_5      = exp(confint.default(model)[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05,   1     ] ),
      CI_97_5     = exp(confint.default(model)[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05,   2     ] ),
      Pvalue      =      summary(model)$coef[ summary(model)$coef[,"Pr(>|z|)"] <= 0.05, "Pr(>|z|)"]  
  )
}

library(glmnet)
library(ROCR)
library(data.table)

# x: the features bucket
# y: the outcome variables bucket
# column_num: the index in y for which to calculate results. the order depends on y. assumption: 
# 1-Current_Suicidal_Ideation
# 2-Lifetime_Suicide_Attempt
# 3-Depression_mod_above_at_phq
run_lasso <- function(x,y, column_num) {
  
  lambdas <- 10^seq(3, -2, by = -.1)
  splits <- 100
  
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
  
  lasso_80_sen = matrix(NA,splits,3)
  colnames(lasso_80_sen) <- paste(colnames(y[,c(1:3)]))
  
  lasso_80_sen_spe = matrix(NA,splits,3)
  colnames(lasso_80_sen_spe) <- paste(colnames(y[,c(1:3)]))
  
  
  #go over every y
  set.seed(42)
  for (j in 1:3){
    
    #split the data splits times to 75% training and 25% test
    for (i in 1:splits) {
      
      splitz <- runif(nrow(x))
      x_train <- x[which(splitz < 0.75),]
      y_train <- y[which(splitz < 0.75),j]
      x_test <- x[which(splitz > 0.75),]
      y_test <- y[which(splitz > 0.75),j]
      
      #find best lambda
      mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=1,family="binomial",lambda=lambdas)
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
      
      #if the model didn't select features, the values of sen will be only 0 and 1 
      #or if the only selected point in the ROC has distance more then 1 (sen will be only 0 or 1) 
      if(length(perf@x.values[[1]]) < 3 | 
         length(perf@x.values[[1]]) == 3 & (sqrt((perf@x.values[[1]][2])^2 + (1-perf@y.values[[1]][2])^2)) >= 1){ #length(which(coefs != 0 )) == 1
        
        lasso_sen[i,j] = 0 
        lasso_spe[i,j] = 1
        next
      }
      
      
      
      #calculate the point(s) on ROC closest to (0,1)
      min_dis = 1
      sen = numeric()
      spe = numeric()
      for (k in 1:length(perf@x.values[[1]])) {
        dis = sqrt((perf@x.values[[1]][k])^2 + (1-perf@y.values[[1]][k])^2)
        if (dis < min_dis) {
          min_dis = dis
          sen = perf@y.values[[1]][k]
          spe = 1 - perf@x.values[[1]][k]
        } else if(dis == min_dis){
          sen[length(sen)+1] = perf@y.values[[1]][k]
          spe[length(spe)+1] = 1 - perf@x.values[[1]][k]
        }
      }
      
      lasso_sen[i,j] = unlist(sen)
      lasso_spe[i,j] = unlist(spe)
      
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
      
    }
  }
  
  #calculate and print results
  cat("Lasso results for", colnames(lasso_auc)[column_num],"\n")
  
  #calculate mean AUC
  cat("\nAUC\n")
  print(apply(lasso_auc, 2, mean, na.rm=TRUE))
  cat("\n")
  
  #get features
  cat("\nFeatures\n")
  features = features[order(features[,column_num], decreasing = TRUE),]
  print(features)
  plot(features[,column_num] ,xlab="" ,ylab="Frequency", xaxt="n" , main=colnames(features)[column_num])
  axis(1, at=1:nrow(features), labels=rownames(features))
  
  # avg. number of selected features. 
  # if there is no clear "knee" in the features graph, use the avg. selected features
  cat("\n Avg. selected features")
  cat("\n all 100 models: " , number_of_features[1,column_num]/100, sep = "\t")  
  cat("\n only models that selected features: " , number_of_features[1,column_num]/number_of_features[2,column_num], sep = "\t")  
  cat("\n")
  
  #get sen and spe closest to (0,1)
  cat("\n Closest point on the ROC to (0,1) ")
  cat("\n Sensitivity \n")
  print(apply(lasso_sen, 2, mean, na.rm=TRUE))
  cat("\n Specificity \n")
  print(apply(lasso_spe, 2, mean, na.rm=TRUE))
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
run_ridge <- function(x,y) {
  set.seed(42)
  lambdas <- 10^seq(3, -2, by = -.1)
  splits <- 100
  
  ridge_auc <- matrix(NA,splits,3)
  colnames(ridge_auc) <- paste(colnames(y[,c(1:3)]))
  
  ridge_sen = matrix(NA,splits,3)
  colnames(ridge_sen) <- paste(colnames(y[,c(1:3)]))
  
  ridge_spe = matrix(NA,splits,3)
  colnames(ridge_spe) <- paste(colnames(y[,c(1:3)]))
  
  ridge_80_sen = matrix(NA,splits,3)
  colnames(ridge_80_sen) <- paste(colnames(y[,c(1:3)]))
  
  ridge_80_sen_spe = matrix(NA,splits,3)
  colnames(ridge_80_sen_spe) <- paste(colnames(y[,c(1:3)]))
  
  
  #go over every y
  for (j in 1:3){
    
    #split the data splits times to 75% training and 25% test
    for (i in 1:splits) {
      
      splitz <- runif(nrow(x))
      x_train <- x[which(splitz < 0.75),]
      y_train <- y[which(splitz < 0.75),j]
      x_test <- x[which(splitz > 0.75),]
      y_test <- y[which(splitz > 0.75),j]
      
      #find best lambda
      mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial",lambda=lambdas)
      opt_lambda <- mod$lambda.min
      
      #get model
      mod <- mod$glmnet.fit
      
      y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
      pred <- prediction(y_predicted, y_test)
      
      #calculate AUC
      ridge_auc[i,j] <- performance(pred, measure = "auc")@y.values[[1]]
      
      #calculate sensitivity and specificity
      perf <- performance(pred,"tpr","fpr")
      
      #if the model didn't select features, the values of sen will be only 0 and 1 
      #or if the only selected point in the ROC has distance more then 1 (sen will be only 0 or 1) 
      if(length(perf@x.values[[1]]) < 3 | 
         length(perf@x.values[[1]]) == 3 & (sqrt((perf@x.values[[1]][2])^2 + (1-perf@y.values[[1]][2])^2)) >= 1){ 
        
        ridge_sen[i,j] = 0 
        ridge_spe[i,j] = 1
        next
      }
      
      #calculate the point(s) on ROC closest to (0,1)
      min_dis = 1
      sen = numeric()
      spe = numeric()
      for (k in 1:length(perf@x.values[[1]])) {
        dis = sqrt((perf@x.values[[1]][k])^2 + (1-perf@y.values[[1]][k])^2)
        if (dis < min_dis) {
          min_dis = dis
          sen = perf@y.values[[1]][k]
          spe = 1 - perf@x.values[[1]][k]
        } else if(dis == min_dis){
          sen[length(sen)+1] = perf@y.values[[1]][k]
          spe[length(spe)+1] = 1 - perf@x.values[[1]][k]
        }
      }
      
      ridge_sen[i,j] = unlist(sen)
      ridge_spe[i,j] = unlist(spe)
      
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
      
    }
  }
  
  #calculate and print results
  cat("Ridge results \n")
  
  #calculate mean AUC
  cat("\nAUC\n")
  print(apply(ridge_auc, 2, mean, na.rm=TRUE))
  cat("\n")
  
  #get sen and spe closest to (0,1)
  cat("\n Closest point on the ROC to (0,1) ")
  cat("\n Sensitivity \n")
  print(apply(ridge_sen, 2, mean, na.rm=TRUE))
  cat("\n Specificity \n")
  print(apply(ridge_spe, 2, mean, na.rm=TRUE))
  cat("\n")
  
  #get sen 80%
  cat("\n Closest point to Sensitivity = 80% ")
  cat("\n Sensitivity \n")
  print(apply(ridge_80_sen, 2, mean, na.rm=TRUE))
  cat("\n Specificity \n")
  print(apply(ridge_80_sen_spe, 2, mean, na.rm=TRUE))
}

