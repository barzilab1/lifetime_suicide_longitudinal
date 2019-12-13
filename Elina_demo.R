
library(psych)
library(parcor)
#library(h2o)

# read in data
x1 <- read.csv("PHQ9_file_for_Elina_with_PNC_FH.csv")
x2 <- read.csv("PNC_Core_Data_demographics.csv")
x3 <- read.csv("PNC_Core_Data_cognitive.csv")
x <- merge(x2,x1,by=1,all=TRUE)
x <- merge(x,x3,by=1,all=TRUE)
x[,27:28] <- x[,27:28]-1
sex <- x$sex
age <- x$AGE_YEARS_AT_CONTACT
cog <- x[,36:61]

# make object to receive data
resids <- matrix(NA,nrow(cog),ncol(cog))

# loop through each column, predict it with all other variables, and take residuals
for (j in 1:ncol(cog)) {
mod <- lm(cog[,j]~as.matrix(cog[,-j])+sex+age,data=cog,na.action=na.exclude)
resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids) <- paste(colnames(x[,36:61]),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(cog)+sex+age,data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids+sex+age,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

sink(paste("output_",as.numeric(Sys.time()),".txt",sep=""))
summary(mod_raw)
summary(mod_resid)
sink()

mod_raw <- lm(PHQ9_Sum~as.matrix(cog)+sex+age,data=x)
mod_resid <- lm(PHQ9_Sum~resids+sex+age,data=x)
summary(mod_raw)
summary(mod_resid)

sink(paste("output_",as.numeric(Sys.time()),".txt",sep=""))
summary(mod_raw)
summary(mod_resid)
sink()




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



library(Amelia)
x <- read.csv("data.csv")
x <- x[,-1]
set.seed(2)
x <- amelia(x,m=1,ords=c(1,2,3))$imputations[[1]]



