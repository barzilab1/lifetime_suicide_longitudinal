library(Amelia)
library(PerformanceAnalytics)

summary(Trauma_bucket)

#sexual assault or attack
table(Trauma_bucket$ptd004, Trauma_bucket$ptd005) #no conflicts
# info is miising in ptd005
which(Trauma_bucket$ptd004 == 1 & is.na(Trauma_bucket$ptd005) ) # [641]

#create new var (4 or 5) and remove ptd004 & ptd005
Trauma_bucket$ptd0045 = Trauma_bucket$ptd004
Trauma_bucket$ptd0045[Trauma_bucket$ptd005 == 1] = 1 
Trauma_bucket = Trauma_bucket[,! names(Trauma_bucket) %in% c("ptd004","ptd005")]

summary(Trauma_bucket)
chart.Correlation(Trauma_bucket[,-1])
boxplot(Trauma_bucket[,-1])

#remove empty rows
length(which(rowSums(is.na(Trauma_bucket)) >= 8)) #5
Trauma_bucket = Trauma_bucket[!(rowSums(is.na(Trauma_bucket)) >= 8),]


set.seed(24)
amelia_fit <- amelia(Trauma_bucket,m=1, idvars=c("bblid","above11"), ords = c(2:8,10))

summary(amelia_fit)

Trauma_bucket_amelia = amelia_fit$imputations[[1]]
describe(Trauma_bucket_amelia)
summary(Trauma_bucket_amelia)
#no need to scale as all features are binary

#Frequency
sum(Trauma_bucket$ptd001, na.rm = TRUE)/nrow(Y_bucket) #0.041
sum(Trauma_bucket$ptd002, na.rm = TRUE)/nrow(Y_bucket) #0.079
sum(Trauma_bucket$ptd003, na.rm = TRUE)/nrow(Y_bucket) #0.026
sum(Trauma_bucket$ptd0045, na.rm = TRUE)/nrow(Y_bucket) #0.016
sum(Trauma_bucket$ptd006, na.rm = TRUE)/nrow(Y_bucket) #0.015
sum(Trauma_bucket$ptd007, na.rm = TRUE)/nrow(Y_bucket) #0.059
sum(Trauma_bucket$ptd008, na.rm = TRUE)/nrow(Y_bucket) #0.101
sum(Trauma_bucket$ptd009, na.rm = TRUE)/nrow(Y_bucket) #0.118

# dt=data.table(Trauma_bucket)
# dt[, lapply(.SD, mean, na.rm=TRUE), by = above11]

#remove above11 for now
Trauma_bucket=Trauma_bucket[,-9]
Trauma_bucket_amelia=Trauma_bucket_amelia[,-9]
#######################################
#Logistic regression 
#######################################

#amelia data set
x = merge(Y_bucket,Trauma_bucket_amelia)
trauma_b = Trauma_bucket_amelia[,-1]

#original data set
x = merge(Y_bucket,Trauma_bucket)
trauma_b = Trauma_bucket[,-1]


### Lifetime_Suicide_Attempt
set.seed(42)
mod_raw <- glm( Lifetime_Suicide_Attempt ~ as.matrix(trauma_b)  ,data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)

# mod_raw <- glm( Lifetime_Suicide_Attempt ~ (ptd001+ptd002+ptd003+ptd0045+ptd006+ptd007+ptd008+ptd009), data=x[x$above11==1,],family="binomial")
# summary(mod_raw)
# get_logistic_results(mod_raw)[-1,]
# pR2(mod_raw)
# visreg(mod_raw,"ptd006")
# visreg(mod_raw,"ptd003")
# visreg(mod_raw,"ptd0045")






### Current_Suicidal_Ideation
# set.seed(42)
# mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(trauma_b),data=x,family="binomial")
# mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
# summary(mod_raw)
# summary(mod_resid)

# set.seed(42)
# mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(trauma_b),data=x,family="binomial")
# mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
# summary(mod_raw)
# summary(mod_resid)


###########################################
#Lasso and ridge with CV
###########################################

#amelia data set
x_total = merge(Y_bucket,Trauma_bucket_amelia)

#original data set
x_total = merge(Y_bucket,Trauma_bucket)
#remove rows with NA 
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)
run_ridge(x,y)



###########################################
#Check Trauma weird results 
###########################################
#make sure Demographics bucket is ready 

#1. calculate AUC for Demographics 
x = merge(Y_bucket,Demographics_bucket)
demo_b = Demographics_bucket_scaled[,-1]

resids = create_resids(demo_b)
x <- data.frame(x,resids)

set.seed(42)
splitz = sample.split(x$Lifetime_Suicide_Attempt, .75)
x_train <- x[splitz,]
demo_b_train = demo_b[splitz,]
x_test <- x[!splitz,]

mod_raw <- glm(Lifetime_Suicide_Attempt~sex+ ethnicity + tml007 + age + goassessPhqDurMonths +race2_White + race2_Black,data=x_train,family="binomial")
summary(mod_raw)
             
y_predicted <- predict(mod_raw, x_test, type="response")
pred <- prediction(y_predicted, x_test$Lifetime_Suicide_Attempt)

#calculate AUC
performance(pred, measure = "auc")@y.values[[1]] #0.5462538


#resid
mod_resid <- glm(Lifetime_Suicide_Attempt~goassessPhqDurMonths_res + sex_res + ethnicity_res + tml007_res +
                  age_res + race2_White_res + race2_Black_res ,data=x,family="binomial")
summary(mod_resid)

y_predicted <- predict(mod_raw, x_test, type="response")
pred <- prediction(y_predicted, x_test$Lifetime_Suicide_Attempt)

#calculate AUC
performance(pred, measure = "auc")@y.values[[1]] #0.5462538


#2. add trauma

x_train = merge(x_train,Trauma_bucket_amelia)
x_test = merge(x_test,Trauma_bucket_amelia)

mod_raw <- glm(Lifetime_Suicide_Attempt~sex+ ethnicity + tml007 + age + goassessPhqDurMonths +race2_White + race2_Black+ 
                 ptd001 + ptd002+ptd003+ptd0045+ptd006+ptd007+ptd008+ptd009,data=x_train,family="binomial")
summary(mod_raw)

y_predicted <- predict(mod_raw, x_test, type="response")
pred <- prediction(y_predicted, x_test$Lifetime_Suicide_Attempt)

#calculate AUC
performance(pred, measure = "auc")@y.values[[1]] #0.5007645
