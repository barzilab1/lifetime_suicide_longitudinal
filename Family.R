library(Amelia)
library(PerformanceAnalytics)
library(parcor)



summary(Family_bucket)
chart.Correlation(Family_bucket[,-1])

#TODO should we handle outliers? before/after imputation? - only edu are affected 
boxplot(Family_bucket[,-1])
boxplot(winsor(Family_bucket[,-1],trim=0.005))

#remove AvgParentEducation. (As avg. has less missing data, check a model with it as well )
Family_bucket = subset(Family_bucket, select=-c(AvgParentEducation))
describe(Family_bucket)

set.seed(24)
amelia_fit <- amelia(Family_bucket,m=1, idvars=c("bblid"), ords = c("Parents_Sep_Divorce",
                                                                    "Ran_substance_FH",
                                                                    "Ran_bipolar_or_lithium_FH",
                                                                    "Ran_psychosis_FH",
                                                                    "Ran_depression_FH",
                                                                    "Ran_Sui_attempt_or_death_FH"))
summary(amelia_fit)

###################################  
#amelia data set
x = merge(Y_bucket,amelia_fit$imputations[[1]])
fam_b = amelia_fit$imputations[[1]][,-1]

#original data set
# x = merge(Y_bucket[,c(1:4)],Family_bucket)
# fam_b = Family_bucket[,-1]

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

mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(fam_b),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(fam_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(fam_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)


###################################  

# CV example

#amelia data set
x_total = merge(Y_bucket,amelia_fit$imputations[[1]])

#original data set
# x_total = merge(Y_bucket,Family_bucket)
# summary(x_total)
# #remove rows with NA
# x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]

y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

set.seed(42)
lambdas <- 10^seq(3, -2, by = -.1)
splits <- 10
results <- matrix(NA,splits,3)
colnames(results) <- paste(colnames(y[,c(1:3)]))

#go over every y
for (j in 1:3){
  
  #split the data splits times to 75% training and 25% test
  for (i in 1:splits) {
    
    splitz <- runif(nrow(x))
    x_train <- x[which(splitz < 0.75),]
    y_train <- y[which(splitz < 0.75),j]
    x_test <- x[which(splitz > 0.75),]
    y_test <- y[which(splitz > 0.75),j]
    
    
    mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial",lambda=lambdas)
    opt_lambda <- mod$lambda.min
    
    mod <- mod$glmnet.fit
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test))
    
    results[i,j] <- cor(cbind(y_predicted,y_test))[2,1]
  }
}
# coef(mod)
# summary(mod)
# mod$coefficients

