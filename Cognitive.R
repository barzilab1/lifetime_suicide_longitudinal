library(PerformanceAnalytics)
library(Amelia)


Cognitive_bucket_combined  = merge(Cognitive_bucket, Cognitive_raw_bucket[,c(1,42,46:59,85,90:101)])
setdiff(Y_bucket$bblid, Cognitive_raw_bucket$bblid) #134866 127485 are missing. together with 136049 they dont have any cognetive data

sum(Cognitive_bucket_combined$er40_validcode_collapsed == "N") #7
sum(Cognitive_bucket_combined$medf_validcode_collapsed == "N") #12
table(Cognitive_bucket_combined$medf_validcode_collapsed, Cognitive_bucket_combined$er40_validcode_collapsed) # 5 = NN , what is S?

#remove the validcode = N, total 14 rows.
Cognitive_bucket_combined = Cognitive_bucket_combined[!(Cognitive_bucket_combined$er40_validcode_collapsed == "N" |
                                                          Cognitive_bucket_combined$medf_validcode_collapsed == "N" ),]
#remove validcode columns 
Cognitive_bucket_combined = Cognitive_bucket_combined[,! names(Cognitive_bucket_combined) %in% 
                                                        c("er40_validcode_collapsed","medf_validcode_collapsed")]

#remove the empty rows
Cognitive_bucket_combined = Cognitive_bucket_combined[rowSums(!is.na(Cognitive_bucket_combined)) >= 26,]

summary(Cognitive_bucket_combined)
# chart.Correlation(Cognitive_bucket_combined[,-1])
boxplot(Cognitive_bucket_combined[,-1])
boxplot(winsor(Cognitive_bucket_combined[,-1],trim=0.005))

#TODO shift outliers. maybe only for the raw data 

write.csv(cor(Cognitive_bucket_combined[,-1], use = "pairwise"),file = "cor.csv")
#er40_cr & eid_ar_z > 0.9
#er40_rt & er40_rtcr > 0.9
#er40_f_cr & er40_cr > 0.87
#er40_f_rtcr & er40_rtcr > 0.89
#er40_f_rtcr & er40_rt > 0.87
#er40_cr & er40_m_cr > 0.85
#er40_m_rtcr & er40_rtcr > .88
#er40_m_rtcr & er40_rt > .86
#edi_ar_z & medf_cr > .9

set.seed(42)
#ords are only the variables that are the # of correct responses, all the reset (z score and response time) are numbers
amelia_fit <- amelia(Cognitive_bucket_combined ,m=1,  idvars=c("bblid","er40_m_cr"), ords = c(28:32,38,42,45,48,51))
# amelia_fit <- amelia(Cognitive_bucket_combined ,m=1,  idvars=c("bblid"), ords = c(28:32,38,40,42,45,48,51))

summary(amelia_fit)
#TODO need to remove er40_m_cr[40]?

##########################################################
#amelia data set
x = merge(Y_bucket[,c(1:4)],amelia_fit$imputations[[1]])
cogni_b = amelia_fit$imputations[[1]][,-1]

#original data set
# x = merge(Y_bucket[,c(1:4)],Clinical_bucket)
# cogni_b = Cognitive_bucket[,-1]

# make object to receive data
resids <- matrix(NA,nrow(cogni_b),ncol(cogni_b))

# loop through each column, predict it with all other variables, and take residuals
for (j in 1:ncol(cogni_b)) {
  mod <- lm(cogni_b[,j]~as.matrix(cogni_b[,-j]),data=cogni_b, na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids) <- paste(colnames(cogni_b),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

#glm.fit: fitted probabilities numerically 0 or 1 occurred
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(cogni_b),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(cogni_b),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(cogni_b),data=x,family="binomial")
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

