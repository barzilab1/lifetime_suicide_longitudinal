library(Amelia)
library(PerformanceAnalytics)
library(parcor)
library(psych)


summary(Clinical_bucket)
t = describe(Clinical_bucket[,-1])
View(t[,-c(1,2)])

write.csv(cor(Clinical_bucket, use = "pairwise"),file = "cor.csv")


# 3 rows have only the summary variables [medical vars that might be assessed by a different doc]  
t = Clinical_bucket[,c(1:114)]
nrow(t[rowSums(is.na(t)) >= 112,])
# 4 rows missing all the summary variables [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
t = Clinical_bucket[,c(115:122)]
nrow(t[rowSums(is.na(t)) == 8,])

set.seed(402)
amelia_fit <- amelia(Clinical_bucket ,m=1,  idvars=c("bblid"), ords = c(2:122))

#TODO hould we handle outliers? before/after imputation? maybe only gaf? 
boxplot(Clinical_bucket[,-c(1,114)])
boxplot(winsor(Clinical_bucket[,-c(1,114)],trim=0.005))
#shift extreme values
# Clinical_bucket_trimmed = winsor(Clinical_bucket[,-1],trim=0.005)
# Clinical_bucket_trimmed = data.frame(Clinical_bucket$bblid, Clinical_bucket_trimmed)
# colnames(Clinical_bucket_trimmed)[1] <- "bblid"
# summary(Clinical_bucket_trimmed)

##########################################################
#amelia data set
x = merge(Y_bucket[,c(1:4)],amelia_fit$imputations[[1]])
clinic_b = amelia_fit$imputations[[1]][,-1]

#original data set
x1 = merge(Y_bucket[,c(1:4)],Clinical_bucket)
clinic_b = Clinical_bucket[,-1]

# make object to receive data
resids <- matrix(NA,nrow(clinic_b),ncol(clinic_b))

# loop through each column, predict it with all other variables, and take residuals
for (j in 1:ncol(clinic_b)) {
  mod <- lm(clinic_b[,j]~as.matrix(clinic_b[,-j]),data=clinic_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids) <- paste(colnames(clinic_b),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

#glm.fit: fitted probabilities numerically 0 or 1 occurred
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(clinic_b),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(clinic_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(clinic_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)


###################################  

# CV example

#amelia data set
x_total = merge(Y_bucket,amelia_fit$imputations[[1]])

#original data set
# x_total = merge(Y_bucket,Clinical_bucket)


#remove empty tows 
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]
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

