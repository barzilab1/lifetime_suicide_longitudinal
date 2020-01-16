library(Amelia)
library(PerformanceAnalytics)
library(parcor)
library(psych)


summary(Demographics_bucket) 

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


summary(Demographics_bucket) 
chart.Correlation(Demographics_bucket[,-1])
describe(Demographics_bucket)
boxplot(Demographics_bucket[,-1])

#shift outliers (edu)
#TODO changes also ages
Demographics_bucket_trimmed = winsor(Demographics_bucket[,-1],trim=0.005)
Demographics_bucket_trimmed = data.frame(Y_bucket$bblid, Demographics_bucket_trimmed)
colnames(Demographics_bucket_trimmed)[1] <- "bblid"
summary(Demographics_bucket_trimmed) 
boxplot(Demographics_bucket_trimmed[,-1])


###################################  
#original data
x = merge(Y_bucket,Demographics_bucket)
demo_b = Demographics_bucket[,-1]

#trimmed data
# x = merge(Y_bucket,Demographics_bucket_trimmed)
# demo_b = Demographics_bucket_trimmed[,-1]

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


mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

######################

# CV example

#original data
x_total = merge(Y_bucket,Demographics_bucket)

#trimmed data
# x_total = merge(Y_bucket,Demographics_bucket_trimmed)

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
    
    ## play with alpha
    mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial",lambda=lambdas)
    opt_lambda <- mod$lambda.min
    
    mod <- mod$glmnet.fit
    
    y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test))
    
    results[i,j] <- cor(cbind(y_predicted,y_test))[2,1]
  }
}



# Sum of Squares Total and Error
# sst <- sum((y - mean(y))^2)
# sse <- sum((y_predicted - y)^2)
# 
# R squared
# rsq <- 1 - sse / sst
# rsq
#> [1] 0.9318896


