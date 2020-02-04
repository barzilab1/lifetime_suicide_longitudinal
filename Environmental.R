library(PerformanceAnalytics)
library(psych)
library(pscl)
library(glmnet)
library(ROCR)


summary(Environment_bucket[,-1])

#remove neighborhoodCrime. 542 out of 922 (59%) are missing
Environment_bucket = Environment_bucket[,! names(Environment_bucket) %in% c("neighborhoodCrime")]

# MedianFamilyIncome <0 , 556 620 => whill be removed as outliers
# Environment_bucket = Environment_bucket[Environment_bucket$MedianFamilyIncome >= 0,]

describe(Environment_bucket[,-1])
chart.Correlation(Environment_bucket[,-1])
boxplot(Environment_bucket[,-1])

#shift outliers values
Environment_bucket_trimmed = winsor(Environment_bucket[,-1],trim=0.005)
Environment_bucket_trimmed = data.frame(Environment_bucket$bblid, Environment_bucket_trimmed)
colnames(Environment_bucket_trimmed)[1] <- "bblid"

describe(Environment_bucket_trimmed[,-1])
summary(Environment_bucket_trimmed[,-1])
boxplot(Environment_bucket_trimmed[,-1])
chart.Correlation(Environment_bucket_trimmed[,-1])


#scale 
Environment_bucket[,-1] = scale(Environment_bucket[,-1]) 
Environment_bucket_trimmed[,-1] = scale(Environment_bucket_trimmed[,-1]) 

#######################################
#trimmed data
x = merge(Y_bucket,Environment_bucket_trimmed)
enviro_b = Environment_bucket_trimmed[,-1]

#original data
x = merge(Y_bucket,Environment_bucket)
enviro_b = Environment_bucket[,-1]


# make object to receive data. remove bblid
resids <- matrix(NA,nrow(enviro_b),ncol(enviro_b))

# loop through each column, predict it with all other variables, and take residuals. except bblid
for (j in 1:ncol(enviro_b)) {
  mod <- lm(enviro_b[,j]~as.matrix(enviro_b[,-j]),data=enviro_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}

# append "res" to column names
colnames(resids) <- paste(colnames(enviro_b),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(enviro_b),data=x,family="binomial")
summary(mod_raw)
pR2(mod_raw)
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
pR2(mod_resid)

set.seed(42)
# mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(enviro_b,data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
# summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)

set.seed(42)
# mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(enviro_b,data=x,family="binomial")
mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
# summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)


###########################################
#Lasso with  CV 

#trimmed data
x_total = merge(Y_bucket,Environment_bucket_trimmed)

#original data
x_total = merge(Y_bucket,Environment_bucket)


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

lambdas <- 10^seq(3, -2, by = -.1)
splits <- 100
lasso_auc <- matrix(NA,splits,3)
colnames(lasso_auc) <- paste(colnames(y[,c(1:3)]))
lambds_max <- matrix(nrow = splits,ncol = 3)

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
    lambds_max[i,j] <- opt_lambda
    mod <- mod$glmnet.fit
    
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
    
    pred <- prediction(y_predicted, y_test)
    lasso_auc[i,j] <- performance(pred, measure = "auc")@y.values[[1]]
  
  }
}


#take the best lanbda for the model
apply(lasso_auc, 2, max, na.rm=TRUE)
inds = apply(lasso_auc, 2, which.max)

#Lifetime_Suicide_Attempt
set.seed(1)
mod = glmnet(x=as.matrix(x), y=y[,2], alpha = 1)
predict(mod, type = "coefficients", s = lambds_max[inds[2],2])

#Current_Suicidal_Ideation
set.seed(1)
mod = glmnet(x=as.matrix(x), y=y[,1], alpha = 1)
predict(mod, type = "coefficients", s = lambds_max[inds[1],1])

#Depression_mod_above_at_phq
set.seed(1)
mod = glmnet(x=as.matrix(x), y=y[,3], alpha = 1)
predict(mod, type = "coefficients", s = lambds_max[inds[3],3])

##########################################
#features selection according to the lasso

####Lifetime_Suicide_Attempt 

#trimmed data
x = merge(Y_bucket,Environment_bucket_trimmed[,c("bblid","MedianFamilyIncome","PWhite", 
                                                 "PercentHighSchoolPlus")])
enviro_b = Environment_bucket_trimmed[,c("MedianFamilyIncome","PWhite", "PercentHighSchoolPlus")]


#original data
x = merge(Y_bucket,Environment_bucket[,c("bblid","MedianFamilyIncome","PWhite", "PercentHighSchoolPlus")])
enviro_b = Environment_bucket[,c("MedianFamilyIncome","PWhite", "PercentHighSchoolPlus")]


# make object to receive data. remove bblid
resids <- matrix(NA,nrow(enviro_b),ncol(enviro_b))

# loop through each column, predict it with all other variables, and take residuals. except bblid
for (j in 1:ncol(enviro_b)) {
  mod <- lm(enviro_b[,j]~as.matrix(enviro_b[,-j]),data=enviro_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}

# append "res" to column names
colnames(resids) <- paste(colnames(enviro_b),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)


set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(enviro_b),data=x,family="binomial")
summary(mod_raw)
pR2(mod_raw)
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
pR2(mod_resid)



####Current_Suicidal_Ideation 
mod_raw <- glm(Current_Suicidal_Ideation ~ PercentEnglishSpeakers_res + PercentHighSchoolPlus_res + 
                                           MedianFamilyIncome_res ,data=x,family="binomial")
summary(mod_raw)
pR2(mod_raw)

mod_raw <- glm(Depression_mod_above_at_phq ~ MedianFamilyIncome + PercentInPoverty + 
                                             PercentNonfamilyHouseholds + PercentWithChildren 
                                             ,data=x,family="binomial")
summary(mod_raw)
pR2(mod_raw)

###########################################
#ridge with  CV 

#trimmed data
x_total = merge(Y_bucket,Environment_bucket_trimmed)

#original data
x_total = merge(Y_bucket,Environment_bucket)


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

set.seed(42)
lambdas <- 10^seq(3, -2, by = -.1)
splits <- 100
ridge_auc <- matrix(NA,splits,3)
colnames(ridge_auc) <- paste(colnames(y[,c(1:3)]))
lambds_max <- matrix(nrow = splits,ncol = 3)

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
    mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial",lambda=lambdas)
    opt_lambda <- mod$lambda.min
    lambds_max[i,j] <- opt_lambda
    mod <- mod$glmnet.fit
    
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
    
    pred <- prediction(y_predicted, y_test)
    ridge_auc[i,j] <- performance(pred, measure = "auc")@y.values[[1]]
    
  }
}


#take the best lanbda for the model
apply(ridge_auc, 2, max, na.rm=TRUE)
inds = apply(ridge_auc, 2, which.max)

#Lifetime_Suicide_Attempt
set.seed(1)
mod = glmnet(x=as.matrix(x), y=y[,2], alpha = 0)
predict(mod, type = "coefficients", s = lambds_max[inds[2],2])

#Current_Suicidal_Ideation
set.seed(1)
mod = glmnet(x=as.matrix(x), y=y[,1], alpha = 0)
predict(mod, type = "coefficients", s = lambds_max[inds[1],1])

#Depression_mod_above_at_phq
set.seed(1)
mod = glmnet(x=as.matrix(x), y=y[,3], alpha = 0)
predict(mod, type = "coefficients", s = lambds_max[inds[3],3])


