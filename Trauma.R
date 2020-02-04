library(Amelia)
library(PerformanceAnalytics)

summary(Trauma_bucket)

#sexual assault or attack
table(Trauma_bucket$ptd005) # total: 14 
table(Trauma_bucket$ptd004) # total: 15
# one is miising in ptd005
which(Trauma_bucket$ptd004 == 1 & is.na(Trauma_bucket$ptd005) ) # [641]

#create new var (4 or 5) and remove ptd004 & ptd005
Trauma_bucket$ptd0045 = Trauma_bucket$ptd004
Trauma_bucket$ptd0045[Trauma_bucket$ptd005 == 1] = 1 
Trauma_bucket = Trauma_bucket[,! names(Trauma_bucket) %in% c("ptd004","ptd005")]

summary(Trauma_bucket)
chart.Correlation(Trauma_bucket[,-1])
boxplot(Trauma_bucket[,-1])

#remove empty rows
count(Trauma_bucket[rowSums(is.na(Trauma_bucket)) >= 8,]) #5
Trauma_bucket = Trauma_bucket[rowSums(!is.na(Trauma_bucket)) >= 8,]


set.seed(24)
amelia_fit <- amelia(Trauma_bucket,m=1, idvars=c("bblid"), ords = c(2:9))

summary(amelia_fit)

Trauma_bucket_amelia = amelia_fit$imputations[[1]]
describe(Trauma_bucket_amelia)
summary(Trauma_bucket_amelia)
#no need to scale as all features are binary

#Frequency
sum(Trauma_bucket$ptd003, na.rm = TRUE)/nrow(Trauma_bucket) #0.026
sum(Trauma_bucket$ptd006, na.rm = TRUE)/nrow(Trauma_bucket) #0.015

##########################################################
#amelia data set
x = merge(Y_bucket[,c(1:4)],Trauma_bucket_amelia)
trauma_b = Trauma_bucket_amelia[,-1]

#original data set
x = merge(Y_bucket[,c(1:4)],Trauma_bucket)
trauma_b = Trauma_bucket[-1]

# make object to receive data
# resids <- matrix(NA,nrow(trauma_b),ncol(trauma_b))

# loop through each column, predict it with all other variables, and take residuals
# for (j in 1:ncol(trauma_b)) {
#   mod <- lm(trauma_b[,j]~as.matrix(trauma_b[,-j]),data=trauma_b,na.action=na.exclude)
#   resids[,j] <- scale(residuals(mod,na.action=na.exclude))
# }  

# append "res" to column names
# colnames(resids) <- paste(colnames(trauma_b),"_res",sep="")

# add residual columns to data frame
# x <- data.frame(x,resids)

set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(trauma_b),data=x,family="binomial")
# mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
# summary(mod_resid)

set.seed(42)
mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(trauma_b),data=x,family="binomial")
# mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
# summary(mod_resid)

set.seed(42)
mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(trauma_b),data=x,family="binomial")
# mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
summary(mod_raw)
# summary(mod_resid)


###################################  

# CV example

#amelia data set
x_total = merge(Y_bucket,Trauma_bucket_amelia)

#original data set
x_total = merge(Y_bucket,Trauma_bucket)
#remove rows with NA 
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

set.seed(4)
lambdas <- 10^seq(3, -2, by = -.1)
splits <- 10
results <- matrix(NA,splits,3)
colnames(results) <- paste(colnames(y[,c(1:3)]))
lambds_max <- matrix(nrow = splits,ncol = 3)


#go over every y
for (j in 1:3){
  
  #split the data splits times to 75% training and 25% test
  for (i in 1:splits) {
    
    splitz <- runif(nrow(x))
    x_train <- x[which(splitz < 0.75),]
    y_train <- y[which(splitz < 0.75),j]
    x_test <- x[which(splitz > 0.75),]
    y_test <- y[which(splitz > 0.75),j]
    
    
    mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=1,family="binomial",lambda=lambdas)
    opt_lambda <- mod$lambda.min
    
    lambds_max[i,j] <- opt_lambda
    
    mod <- mod$glmnet.fit
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test))
    
    results[i,j] <- (cor(cbind(y_predicted,y_test))[2,1])^2
  }
}

apply(results,2, mean,na.rm=TRUE)


#take the best lanbda for the model
apply(results, 2, max, na.rm=TRUE)
inds = apply(results, 2, which.max)

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

##########################################################
#amelia data set
x = merge(Y_bucket[,c(1:4)],Trauma_bucket_amelia)
trauma_b = Trauma_bucket_amelia[,-1]

#original data set
x = merge(Y_bucket[,c(1:4)],Trauma_bucket)
trauma_b = Trauma_bucket[-1]

set.seed(12)
mod_raw <- glm(Lifetime_Suicide_Attempt ~ ptd006 + ptd0045,data=x,family="binomial")
summary(mod_raw)

set.seed(12)
mod_raw <- glm(Depression_mod_above_at_phq~ptd003 ,data=x,family="binomial")
summary(mod_raw)
