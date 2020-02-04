library(PerformanceAnalytics)
library(psych)
library(pscl)
library(glmnet)
library(ROCR)


summary(Demographics_bucket[,-1]) 
describe(Demographics_bucket[,-1])

#missing edu1 will be the mean of the edu of kids at the same age 
na_edu_indexes = which(is.na(Demographics_bucket$edu1))

for ( i in na_edu_indexes){
  Demographics_bucket$edu1[i] = round(mean(Demographics_bucket$edu1[
    Demographics_bucket$ageAtClinicalAssess1 == Demographics_bucket$ageAtClinicalAssess1[i]], na.rm = TRUE),2)
}


#add the phq age
Demographics_bucket$ageAtPHQAssess2 = Demographics_bucket$ageAtClinicalAssess1 + Demographics_bucket$goassessPhqDurMonths
#remove goassessPhqDurMonths
Demographics_bucket = subset(Demographics_bucket, select=-c(goassessPhqDurMonths))


#missing cnb age will be the goassess1 age
Demographics_bucket$ageAtCnb1[is.na(Demographics_bucket$ageAtCnb1)] = 
  Demographics_bucket$ageAtClinicalAssess1[is.na(Demographics_bucket$ageAtCnb1)]

#get mean of age at goassess1 and cnb ( we can't use both)
Demographics_bucket$age = rowMeans(data.frame(Demographics_bucket$ageAtClinicalAssess1, 
                                              Demographics_bucket$ageAtCnb1))

#remove the other age features
Demographics_bucket = subset(Demographics_bucket, select=-c(ageAtClinicalAssess1,ageAtCnb1))


#create 2 variables for race2 
Demographics_bucket$race2_White = ifelse(Demographics_bucket$race2 == 1 , 1, 0)
Demographics_bucket$race2_Black = ifelse(Demographics_bucket$race2 == 2 , 1, 0)
#remove race2
Demographics_bucket = subset(Demographics_bucket, select=-c(race2))

#change sex and ethnicity range from [1,2] to [0,1]
Demographics_bucket[,c("sex","ethnicity")] =  Demographics_bucket[,c("sex","ethnicity")] -1


summary(Demographics_bucket[,-1]) 
chart.Correlation(Demographics_bucket[,-1])
describe(Demographics_bucket[,-1])
boxplot(Demographics_bucket[,-1])

#scale only the non-binary features 
Demographics_bucket[,4:6] = scale(Demographics_bucket[,4:6]) 
describe(Demographics_bucket[,-1])

#Frequency
sum(Demographics_bucket$sex)/nrow(Demographics_bucket) #0.476
sum(Demographics_bucket$ethnicity)/nrow(Demographics_bucket) #0.937
sum(Demographics_bucket$race2_White)/nrow(Demographics_bucket) #0.400

###################################  
x = merge(Y_bucket,Demographics_bucket)
demo_b = Demographics_bucket[,-1]

# make object to receive data
resids <- matrix(NA,nrow(demo_b),ncol(demo_b))

# loop through each column, predict it with all other variables, and take residuals
for (j in 1:ncol(demo_b)) {
  mod <- lm(demo_b[,j]~as.matrix(demo_b[,j]),data=demo_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}

# append "res" to column names
colnames(resids) <- paste(colnames(demo_b),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

set.seed(42)
# mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
# summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)

set.seed(42)
mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)

set.seed(42)
# mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
# summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)


###########################################
#Lasso with  CV

#original data
x_total = merge(Y_bucket,Demographics_bucket)

y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

set.seed(42)
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

x = merge(Y_bucket,Demographics_bucket)
demo_b = Demographics_bucket[,-1]

# make object to receive data
resids <- matrix(NA,nrow(demo_b),ncol(demo_b))

# loop through each column, predict it with all other variables, and take residuals
for (j in 1:ncol(demo_b)) {
  mod <- lm(demo_b[,j]~as.matrix(demo_b[,j]),data=demo_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}

# append "res" to column names
colnames(resids) <- paste(colnames(demo_b),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)


set.seed(2)
#features selection accorsing to the lasso
mod_raw <- glm(Lifetime_Suicide_Attempt~race2_White_res + sex_res ,data=x,family="binomial")
summary(mod_raw)
pR2(mod_raw)

mod_raw <- glm(Current_Suicidal_Ideation~race2_White_res,data=x,family="binomial")
summary(mod_raw)
pR2(mod_raw)

mod_raw <- glm(Depression_mod_above_at_phq~sex_res ,data=x,family="binomial")
summary(mod_raw)
pR2(mod_raw)


###########################################
#ridge with  CV 

#original data
x_total = merge(Y_bucket,Demographics_bucket)


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





