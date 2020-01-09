library(PerformanceAnalytics)

summary(Environment_bucket)
#TODO what to do with neighborhoodCrime? 544 out of 929 (59%) are missing
chart.Correlation(Environment_bucket[,-1], histogram=TRUE, pch=19)


##################
x = merge(Y_bucket,Environment_bucket)

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


######################
x = merge(Y_bucket,Environment_bucket[,-16])
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

