library(PerformanceAnalytics)
library(Amelia)


Cognitive_bucket_combined  = merge(Cognitive_bucket, Cognitive_raw_bucket[,c(1,42,46:59,85,90:101)])
setdiff(Y_bucket$bblid, Cognitive_raw_bucket$bblid) #134866 127485 are missing. together with 136049 they dont have any cognetive data

sum(Cognitive_bucket_combined$er40_validcode_collapsed == "N") #7
sum(Cognitive_bucket_combined$er40_validcode_collapsed == "S") #3 (empty)
sum(Cognitive_bucket_combined$medf_validcode_collapsed == "N") #12
sum(Cognitive_bucket_combined$medf_validcode_collapsed == "S") #19 (empty)
table(Cognitive_bucket_combined$medf_validcode_collapsed, Cognitive_bucket_combined$er40_validcode_collapsed) 

#change tests with validcode = N to NA, total 14 rows. 
Cognitive_bucket_combined[Cognitive_bucket_combined$er40_validcode_collapsed == "N",29:42] = NA 
Cognitive_bucket_combined[Cognitive_bucket_combined$medf_validcode_collapsed == "N",44:55] = NA 

#remove validcode columns 
Cognitive_bucket_combined = Cognitive_bucket_combined[,! names(Cognitive_bucket_combined) %in% 
                                                        c("er40_validcode_collapsed","medf_validcode_collapsed")]

# % of NA. missing 2 rows that appear in the other buckets
number_NA = sum(is.na(Cognitive_bucket_combined[,-1])) #687
data_dim = dim(Cognitive_bucket_combined[,-1]) 
number_NA/(data_dim[1]*data_dim[2]) #0.14

#remove empty rows
Cognitive_bucket_combined = Cognitive_bucket_combined[rowSums(!is.na(Cognitive_bucket_combined)) >= 26,]

summary(Cognitive_bucket_combined[,-1])
# chart.Correlation(Cognitive_bucket_combined[,-1])
boxplot(Cognitive_bucket_combined[,-1])

# take the log for response time
Z = sapply(Cognitive_bucket_combined[,c(33:37,39,41,43,44,46,47,49,50,52,53)], log)
colnames(Z) <- paste(colnames(Z),"_log",sep="")
Cognitive_bucket_combined = data.frame(Cognitive_bucket_combined,Z)
Cognitive_bucket_combined = Cognitive_bucket_combined[,-c(33:37,39,41,43,44,46,47,49,50,52,53)]

boxplot(Cognitive_bucket_combined[,-1])

write.csv(cor(Cognitive_bucket_combined[,-1], use = "pairwise"),file = "cor_cog.csv")
#er40_f_rtcr_log & eid_s_ar_z < -.81
#er40_m_rtcr_log & eid_s_ar_z < -.80
#er40_m_cr       & eid_ar_z   > .83
#er40_f_cr       & eid_ar_z   > .79
#medf_hap_cr     & edi_ar_z   > .74
#medf_ang_cr     & edi_ar_z   > .72
#medf_fear_cr    & edi_ar_z   > .72
#medf_hap_rtcr_log  & edi_s_ar_z < -.75
#medf_ang_rtcr_log  & edi_s_ar_z < -.79
#medf_fear_rtcr_log & edi_s_ar_z < -.74
#medf_sad_rtcr_log  & edi_s_ar_z < -.7


set.seed(42)
#all the variables are numbers
amelia_fit <- amelia(Cognitive_bucket_combined ,m=1,  idvars=c("bblid","er40_m_cr"))

summary(amelia_fit)

##########################################################
#amelia data set
x = merge(Y_bucket[,c(1:4)],amelia_fit$imputations[[1]])
cogni_b = amelia_fit$imputations[[1]][,-1]

#original data set
x = merge(Y_bucket[,c(1:4)],Cognitive_bucket_combined)
cogni_b = Cognitive_bucket_combined[,-1]

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
mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)


###################################  

# CV example

#amelia data set
x_total = merge(Y_bucket,amelia_fit$imputations[[1]])

#original data set
x_total = merge(Y_bucket,Cognitive_bucket_combined)


#remove empty rows 
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]
y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

set.seed(42)
lambdas <- 10^seq(3, -2, by = -.1)
splits <- 100
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
    
    results[i,j] <- (cor(cbind(y_predicted,y_test))[2,1])^2
  }
}

apply(results,2, mean)
# coef(mod)
# summary(mod)
# mod$coefficients

