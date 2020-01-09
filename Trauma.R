library(Amelia)
library(PerformanceAnalytics)

summary(Trauma_bucket)
chart.Correlation(Trauma_bucket[,-1], histogram=TRUE, pch=19)

table(Trauma_bucket$ptd005) # total: 14 
table(Trauma_bucket$ptd004) # total: 15
# one is miising in ptd005
which(Trauma_bucket$ptd004 == 1 & is.na(Trauma_bucket$ptd005) ) # [641]
#TODO ask! create new var (4 or 5)?
#first populate with 0. amelia keeps mean so populates with a lot of 1
#fill all NA in ptd005 with 0: if wasn't ask ptd004 == 0, then it is 0 else [641]? 
# Trauma_bucket$ptd005[is.na(Trauma_bucket$ptd005)] = 0

#create new var (4 or 5)?
Trauma_bucket$ptd0045 = Trauma_bucket$ptd004
Trauma_bucket$ptd0045[Trauma_bucket$ptd005 == 1] = 1 
Trauma_bucket = Trauma_bucket[,! names(Trauma_bucket) %in% c("ptd004","ptd005")]


summary(Trauma_bucket)
chart.Correlation(Trauma_bucket[,-1], histogram=TRUE, pch=19)

#also removes empty rows 
amelia_fit <- amelia(Trauma_bucket,idvars=c("bblid"), ords = c(2:9))

summary(amelia_fit)

##########################################################
#amelia data set
trauma_b = amelia_fit$imputations[[1]]
x = merge(Y_bucket[,c(1:4)],trauma_b)
# make object to receive data
resids <- matrix(NA,nrow(trauma_b),ncol(trauma_b)-1)

# loop through each column, predict it with all other variables, and take residuals
for (i in 2:ncol(trauma_b)) {
  j = i-1
  mod <- lm(trauma_b[,j]~as.matrix(trauma_b[,-c(1,j)]),data=trauma_b,na.action=na.exclude)
  resids[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids) <- paste(colnames(trauma_b[,-1]),"_res",sep="")

# add residual columns to data frame
x <- data.frame(x,resids)

#glm.fit: fitted probabilities numerically 0 or 1 occurred
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(trauma_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(trauma_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(trauma_b[,-c(1)]),data=x,family="binomial")
mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
summary(mod_raw)
summary(mod_resid)

#################
#original data set
x1 = merge(Y_bucket[,c(1:4)],Trauma_bucket)
# make object to receive data
resids_orig <- matrix(NA,nrow(Trauma_bucket),ncol(Trauma_bucket)-1)

# loop through each column, predict it with all other variables, and take residuals
for (i in 2:ncol(Trauma_bucket)) {
  j = i-1
  mod <- lm(Trauma_bucket[,j]~as.matrix(Trauma_bucket[,-c(1,j)]),data=Trauma_bucket,na.action=na.exclude)
  resids_orig[,j] <- scale(residuals(mod,na.action=na.exclude))
}  

# append "res" to column names
colnames(resids_orig) <- paste(colnames(Trauma_bucket[,-1]),"_res",sep="")

# add residual columns to data frame
x1 <- data.frame(x,resids_orig)

#glm.fit: fitted probabilities numerically 0 or 1 occurred
mod_raw_orig <- glm(Lifetime_Suicide_Attempt~as.matrix(Trauma_bucket[,-c(1)]),data=x1,family="binomial")
mod_resid_orig <- glm(Lifetime_Suicide_Attempt~resids_orig,data=x1,family="binomial")
summary(mod_raw_orig)
summary(mod_resid_orig)

mod_raw_orig <- glm(Current_Suicidal_Ideation~as.matrix(Trauma_bucket[,-c(1)]),data=x1,family="binomial")
mod_resid_orig <- glm(Current_Suicidal_Ideation~resids_orig,data=x1,family="binomial")
summary(mod_raw_orig)
summary(mod_resid_orig)

mod_raw_orig <- glm(Depression_mod_above_at_phq~as.matrix(Trauma_bucket[,-c(1)]),data=x1,family="binomial")
mod_resid_orig <- glm(Current_Suicidal_Ideation~resids_orig,data=x1,family="binomial")
summary(mod_raw_orig)
summary(mod_resid_orig)

