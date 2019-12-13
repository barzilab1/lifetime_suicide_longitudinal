library(Amelia)
library(PerformanceAnalytics)
library(parcor)


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

#get mean of age at goassess1 and cnb
Demographics_bucket$age = rowMeans(data.frame(Demographics_bucket$ageAtClinicalAssess1, 
                                              Demographics_bucket$ageAtCnb1))

#remove the other age features
Demographics_bucket = subset(Demographics_bucket, select=-c(ageAtClinicalAssess1,ageAtCnb1))


#create 2 variables for race 2 
Demographics_bucket$race2_White = ifelse(Demographics_bucket$race2 == 1 , 1, 0)
Demographics_bucket$race2_Black = ifelse(Demographics_bucket$race2 == 2 , 1, 0)
#remove race2
Demographics_bucket = subset(Demographics_bucket, select=-c(race2))


summary(Demographics_bucket) 


#correllation matrix
cormat = cor(Demographics_bucket)
melted_cormat = melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
#pairs.panels(Demographics_bucket, scale=TRUE)

chart.Correlation(Demographics_bucket[,-1], histogram=TRUE, pch=19)


################################### TEST 

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

#over fitted. use different model
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(demo_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

######################

# CV example

# read in data
x1 <- read.csv("PHQ9_file_for_Elina_with_PNC_FH.csv")
x2 <- read.csv("PNC_Core_Data_demographics.csv")
x3 <- read.csv("PNC_Core_Data_cognitive.csv")
x <- merge(x2,x1,by=1,all=TRUE)
x <- merge(x,x3,by=1,all=TRUE)
x <- x[which(is.na(x$Lifetime_Suicide_Attempt) == FALSE),]
x <- x[which(is.na(x$abf_ar_z) == FALSE),]
x[,27:28] <- x[,27:28]-1
sex <- x$sex
age <- x$AGE_YEARS_AT_CONTACT
cog <- x[,36:61]
lambdas <- 10^seq(3, -2, by = -.1)
splits <- 10

y <- x$Lifetime_Suicide_Attempt
x <- data.frame(cog,sex,age)
results <- rep(NA,splits)

for (i in 1:splits) {
  
  splitz <- runif(nrow(x))
  x_train <- x[which(splitz < 0.75),]
  y_train <- y[which(splitz < 0.75)]
  x_test <- x[which(splitz > 0.75),]
  y_test <- y[which(splitz > 0.75)]
  
  
  ## play with alpha
  mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial",lambda=lambdas)
  opt_lambda <- mod$lambda.min
  
  mod <- mod$glmnet.fit
  
  y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test))
  
  results[i] <- cor(cbind(y_predicted,y_test))[2,1]
  results[results <0] <- 0
  results <- results^2
  
}







# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq
#> [1] 0.9318896






## random forest tree
