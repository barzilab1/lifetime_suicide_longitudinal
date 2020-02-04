library(Amelia)
library(PerformanceAnalytics)
library(parcor)



summary(Family_bucket)
chart.Correlation(Family_bucket[,-1])

boxplot(Family_bucket[,-1])
# boxplot(winsor(Family_bucket[,-1],trim=0.005))

#remove parents education. (As avg. has less missing data)
Family_bucket = subset(Family_bucket, select=-c(medu1,fedu1 ))
describe(Family_bucket[,-1])

set.seed(24)
amelia_fit <- amelia(Family_bucket,m=1, idvars=c("bblid"), ords = c("Parents_Sep_Divorce",
                                                                    "Ran_substance_FH",
                                                                    "Ran_bipolar_or_lithium_FH",
                                                                    "Ran_psychosis_FH",
                                                                    "Ran_depression_FH",
                                                                    "Ran_Sui_attempt_or_death_FH"))
summary(amelia_fit)
Family_bucket_amelia = amelia_fit$imputations[[1]]
describe(Family_bucket_amelia[,-1])

#scale only the non-binary features 
Family_bucket_amelia[,7] = scale(Family_bucket_amelia[,7]) 
Family_bucket[,7] = scale(Family_bucket[,7]) 

describe(Family_bucket_amelia[,-1])


#Frequency
sum(Family_bucket$Ran_substance_FH, na.rm = TRUE)/nrow(Family_bucket) #0.1475
sum(Family_bucket$Ran_depression_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0965
sum(Family_bucket$Ran_bipolar_or_lithium_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0249
sum(Family_bucket$Ran_psychosis_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0293
sum(Family_bucket$Ran_Sui_attempt_or_death_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0553
sum(Family_bucket$Parents_Sep_Divorce, na.rm = TRUE)/nrow(Family_bucket) #0.167

###################################  
#amelia data set
x = merge(Y_bucket,Family_bucket_amelia)
fam_b = Family_bucket_amelia[,-1]

#original data set
x = merge(Y_bucket[,c(1:4)],Family_bucket)
fam_b = Family_bucket[,-1]

# make object to receive data
resids <- matrix(NA,nrow(fam_b),ncol(fam_b))

# loop through each column, predict it with all other variables, and take residuals
for (j in 1:ncol(fam_b)) {
mod <- lm(fam_b[,j]~as.matrix(fam_b[,-j]),data=fam_b,na.action=na.exclude)
resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}

# append "res" to column names
colnames(resids) <- paste(colnames(fam_b[]),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

set.seed(24)
# mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(fam_b),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
# summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)

set.seed(24)
# mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(fam_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
# summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)

set.seed(24)
# mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(fam_b),data=x,family="binomial")
mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
# summary(mod_raw)
summary(mod_resid)
pR2(mod_resid)


###########################################
#Lasso with  CV

#amelia data set
x_total = merge(Y_bucket,Family_bucket_amelia)

#original data set
x_total = merge(Y_bucket,Family_bucket)
# summary(x_total)
# remove rows with NA
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

set.seed(402)
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

#amelia data set
x = merge(Y_bucket,Family_bucket_amelia)
fam_b = Family_bucket_amelia[,-1]

#original data set
x = merge(Y_bucket[,c(1:4)],Family_bucket)
fam_b = Family_bucket[,-1]


# make object to receive data
resids <- matrix(NA,nrow(fam_b),ncol(fam_b))

# loop through each column, predict it with all other variables, and take residuals
for (j in 1:ncol(fam_b)) {
  mod <- lm(fam_b[,j]~as.matrix(fam_b[,-j]),data=fam_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}

# append "res" to column names
colnames(resids) <- paste(colnames(fam_b[]),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

set.seed(2)
#features selection accorsing to the lasso
mod_raw <- glm(Lifetime_Suicide_Attempt ~ Parents_Sep_Divorce_res + Ran_Sui_attempt_or_death_FH_res + 
                                          Ran_substance_FH_res + AvgParentEducation_res + 
                                          Ran_bipolar_or_lithium_FH_res, data=x,family="binomial")
summary(mod_raw)

mod_raw <- glm(Current_Suicidal_Ideation ~ Ran_psychosis_FH_res + AvgParentEducation_res,
                                           data=x,family="binomial")
summary(mod_raw)

mod_raw <- glm(Depression_mod_above_at_phq ~ Ran_bipolar_or_lithium_FH_res + 
                                             Ran_Sui_attempt_or_death_FH_res + 
                                             Ran_depression_FH_res + AvgParentEducation_res 
                                             ,data=x,family="binomial")
summary(mod_raw)


###########################################
#ridge with  CV 

#amelia data set
x_total = merge(Y_bucket,Family_bucket_amelia)

#original data set
x_total = merge(Y_bucket,Family_bucket)
# summary(x_total)
# remove rows with NA
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]



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




