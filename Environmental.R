library(PerformanceAnalytics)


summary(Environment_bucket)
#remove neighborhoodCrime. 542 out of 922 (59%) are missing
Environment_bucket = Environment_bucket[,! names(Environment_bucket) %in% c("neighborhoodCrime")]
#remove MedianFamilyIncome <0 , 556 620 => -2906.436 -13763.194
Environment_bucket = Environment_bucket[Environment_bucket$MedianFamilyIncome >= 0,]
describe(Environment_bucket)
chart.Correlation(Environment_bucket[,-1])
boxplot(Environment_bucket[,-1])

#shift outliers values
Environment_bucket_trimmed = winsor(Environment_bucket[,-1],trim=0.005)
Environment_bucket_trimmed = data.frame(Environment_bucket$bblid, Environment_bucket_trimmed)
colnames(Environment_bucket_trimmed)[1] <- "bblid"
summary(Environment_bucket_trimmed)
boxplot(Environment_bucket_trimmed[,-1])

#######################################
#original data
x = merge(Y_bucket,Environment_bucket)

#trimmed data
# x = merge(Y_bucket,Environment_bucket_trimmed)


# make object to receive data. remove bblid
resids <- matrix(NA,nrow(Environment_bucket),ncol(Environment_bucket)-1)

# loop through each column, predict it with all other variables, and take residuals. except bblid
for (i in 2:ncol(Environment_bucket)) {
  j = i-1
  mod <- lm(Environment_bucket[,j]~as.matrix(Environment_bucket[,-c(1,j)]),data=Environment_bucket,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids) <- paste(colnames(Environment_bucket[,-1]),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(Environment_bucket[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(Environment_bucket[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(Environment_bucket[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)


###########################################

# CV example

#original data
x_total = merge(Y_bucket,Environment_bucket)

#trimmed data
# x_total = merge(Y_bucket,Environment_bucket_trimmed)

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
