library(Amelia)
library(PerformanceAnalytics)
library(parcor)
library(psych)


summary(Clinical_bucket)
# describe(Clinical_bucket[,-1])
# View(t[,-c(1,2)])

write.csv(cor(Clinical_bucket, use = "pairwise"),file = "cor_clinic.csv")
#smry_psych_medication_rtg & scr006 > .9


# 3 rows have only the summary variables [medical vars that might be assessed by a different doc]  
t = Clinical_bucket[,c(1:114)]
nrow(t[rowSums(is.na(t)) >= 112,])
# 4 rows missing all the summary variables [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
t = Clinical_bucket[,c(115:122)]
nrow(t[rowSums(is.na(t)) == 8,])


# #add indicator for age >= 11
# Clinical_bucket$above11 = ifelse(Clinical_bucket$ageAtClinicalAssess1 >= 132,1,0)
# #remove ageAtClinicalAssess1
# Clinical_bucket = subset(Clinical_bucket, select=-c(ageAtClinicalAssess1))

#TODO should we handle outliers? No
boxplot(Clinical_bucket[,c(114)])

#########
#Substance
summary(Substance_bucket)

#total isn't a summary.  
t = GO1_Substance_Use[,c("subs_smry_sub_alc","subs_smry_sub_mar")]
t$remaining_sum = GO1_Substance_Use$subs_smry_sub_tot - rowSums(GO1_Substance_Use[,c(78:86)],na.rm = TRUE)
t$subs_smry_sub_mar[is.na(t$subs_smry_sub_mar)] = 0
lm(data=t[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc)),], remaining_sum ~ subs_smry_sub_mar )
which(t$subs_smry_sub_mar[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc))] != t$remaining_sum[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc))])

#check frequser of marichuana of the entire dataset - ok. 
table(GO1_Substance_Use$MarUse, GO1_Substance_Use$subs_smry_sub_mar)
table(Substance_bucket$MarUse, Substance_bucket$subs_smry_sub_mar) #total 4 users

#check frequser of alcohol of the entire dataset - not ok. stick with AlcUse 
table(GO1_Substance_Use$AlcUse, GO1_Substance_Use$subs_smry_sub_alc)
table(Substance_bucket$AlcUse, Substance_bucket$subs_smry_sub_alc)

#remove subs_smry_sub_alc and subs_smry_sub_tran (missing 66%) and subs_smry_sub_tot
Substance_bucket = Substance_bucket[,! names(Substance_bucket) %in% c("subs_smry_sub_alc", "subs_smry_sub_tran", "subs_smry_sub_tot")]

#convert charecter to numeric
Substance_bucket$MarUse = ifelse(Substance_bucket$MarUse == "nonuser" , 0 , 
                          ifelse(Substance_bucket$MarUse == "user", 1, 2))

Substance_bucket$AlcUse = ifelse(Substance_bucket$AlcUse == "nonuser" , 0 ,1) 

#take only substance with more 1% frequency  
Substance_bucket = Substance_bucket[,(apply(Substance_bucket, 2, sum, na.rm=TRUE) >=9)]


summary(Substance_bucket)
chart.Correlation(Substance_bucket[,-1])


######edu
#how many repeat a grade
sum(t$dem107, na.rm = TRUE) #82
#how many repeat more than 1 class
length(which(t$dem107 == 1 & t$dem108 >1)) #8
#how many repeat only 1 class
length(which(t$dem107 == 1 & t$dem108 <=1)) #74

#get reasons 
write.csv(t[which(t$dem107 == 1), ],file = "repeat_a_grade.csv")


set.seed(402)
amelia_fit <- amelia(Clinical_bucket ,m=1,  idvars=c("bblid","above11"), ords = c(2:122))



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
mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
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

