library(Amelia)
library(Zelig)
library(PerformanceAnalytics)
library(parcor)



summary(Clinical_bucket)
# too big for chart.Correlation(Clinical_bucket[,-1], histogram=TRUE, pch=19)
write.csv(cor(Clinical_bucket, use = "pairwise"),file = "cor.csv")


#TODO What to do with these 3 rows?  
# 3 rows have only the summary variables [medical vars that might be assessed by a different doc]  
t = Clinical_bucket[,c(2:114)]
nrow(t[rowSums(is.na(t)) >= 112,])
# 4 rows missing all the summary variables [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
t = Clinical_bucket[,c(115:122)]
nrow(t[rowSums(is.na(t)) == 8,])
#remove rows with too many NA 
# x = Clinical_bucket[rowSums(is.na(Clinical_bucket)) < 8,]


set.seed(402)
#TODO 2-113 boolean, 114(gaf) ord, 115-122: ord?
amelia_fit <- amelia(Clinical_bucket ,m=1,  idvars=c("bblid"), ords = c(2:122))

##########################################################
#amelia data set
clinic_b = amelia_fit$imputations[[1]]
x = merge(Y_bucket[,c(1:4)],clinic_b)
# make object to receive data
resids <- matrix(NA,nrow(clinic_b),ncol(clinic_b)-1)

# loop through each column, predict it with all other variables, and take residuals
for (i in 2:ncol(clinic_b)) {
  j = i-1
  mod <- lm(clinic_b[,j]~as.matrix(clinic_b[,-c(1,j)]),data=clinic_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids) <- paste(colnames(clinic_b[,-1]),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

#glm.fit: fitted probabilities numerically 0 or 1 occurred
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(clinic_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(clinic_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(clinic_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

#################
#original data set
x1 = merge(Y_bucket[,c(1:4)],Clinical_bucket)
# make object to receive data
resids_orig <- matrix(NA,nrow(Clinical_bucket),ncol(Clinical_bucket)-1)

# loop through each column, predict it with all other variables, and take residuals
for (i in 2:ncol(Clinical_bucket)) {
  j = i-1
  mod <- lm(Clinical_bucket[,j]~as.matrix(Clinical_bucket[,-c(1,j)]),data=Clinical_bucket,na.action=na.exclude)
  resids_orig[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids_orig) <- paste(colnames(Clinical_bucket[,-1]),"_res",sep="")

# add residual columns to data frame
x1 <- data.frame(x,resids_orig)

#glm.fit: fitted probabilities numerically 0 or 1 occurred
mod_raw_orig <- glm(Lifetime_Suicide_Attempt~as.matrix(Clinical_bucket[,-c(1)]),data=x1,family="binomial")
mod_resid_orig <- glm(Lifetime_Suicide_Attempt~resids_orig,data=x1,family="binomial")
summary(mod_raw_orig)
summary(mod_resid_orig)

mod_raw_orig <- glm(Current_Suicidal_Ideation~as.matrix(Clinical_bucket[,-c(1)]),data=x1,family="binomial")
mod_resid_orig <- glm(Current_Suicidal_Ideation~resids_orig,data=x1,family="binomial")
summary(mod_raw_orig)
summary(mod_resid_orig)

mod_raw_orig <- glm(Depression_mod_above_at_phq~as.matrix(Clinical_bucket[,-c(1)]),data=x1,family="binomial")
mod_resid_orig <- glm(Current_Suicidal_Ideation~resids_orig,data=x1,family="binomial")
summary(mod_raw_orig)
summary(mod_resid_orig)