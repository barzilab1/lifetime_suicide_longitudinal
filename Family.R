library(Amelia)
library(PerformanceAnalytics)
library(parcor)



summary(Family_bucket)
chart.Correlation(Family_bucket[,-1], histogram=TRUE, pch=19)

set.seed(24)

#warning: chol(): given matrix is not symmetric
amelia_fit <- amelia(Family_bucket,idvars=c("bblid"), noms = c("Parents_Sep_Divorce",
                                                               "Ran_substance_FH",
                                                               "Ran_bipolar_or_lithium_FH",
                                                               "Ran_psychosis_FH",
                                                               "Ran_depression_FH",
                                                               "Ran_Sui_attempt_or_death_FH"))




summary(amelia_fit)
# x = merge(Y_bucket,Family_bucket)
fam_b = amelia_fit$imputations[[5]]
x = merge(Y_bucket,fam_b)

# make object to receive data
resids <- matrix(NA,nrow(fam_b),ncol(fam_b)-1)

# loop through each column, predict it with all other variables, and take residuals
for (i in 2:ncol(fam_b)) {
  j = i-1
  mod <- lm(fam_b[,j]~as.matrix(fam_b[,-c(1,j)]),data=fam_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids) <- paste(colnames(fam_b[,-1]),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(fam_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(fam_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(fam_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)
######################

# CV example

x = merge(Y_bucket,fam_b)
# x = x[!is.na(x$Ran_substance_FH ),]
# x = x[!is.na(x$fedu1 ),]
# x = x[!is.na(x$medu1 ),]
# x = x[!is.na(x$Parents_Sep_Divorce ),]
# x = x[!is.na(x$Ran_Sui_attempt_or_death_FH),] 
x = x[!is.na(x$Current_Suicidal_Ideation),]

summary(x)
y = x$Current_Suicidal_Ideation
x = x[,-c(1:5)]


set.seed(42)
lambdas <- 10^seq(3, -2, by = -.1)
splits <- 10
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
  #results[results <0] <- 0
  #results <- results^2
  
}

# coef(mod)
# summary(mod)
# mod$coefficients

