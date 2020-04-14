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
# chart.Correlation(Trauma_bucket[,-1])
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


#######################################
#Logistic regression 
#######################################

# #amelia data set
# #TODO remove above 11
# x = merge(Y_bucket,Trauma_bucket_amelia)
# trauma_b = Trauma_bucket_amelia[,-1]
# 
# #original data set
# x = merge(Y_bucket,Trauma_bucket)
# trauma_b = Trauma_bucket[,-1]
# 
# 
# ### Lifetime_Suicide_Attempt
# set.seed(42)
# mod_raw <- glm( Lifetime_Suicide_Attempt ~ as.matrix(trauma_b)  ,data=x,family="binomial")
# summary(mod_raw)
# get_logistic_results(mod_raw)[-1,]
# pR2(mod_raw)

# mod_raw <- glm( Lifetime_Suicide_Attempt ~ (ptd001+ptd002+ptd003+ptd0045+ptd006+ptd007+ptd008+ptd009), data=x[x$above11==1,],family="binomial")
# summary(mod_raw)
# get_logistic_results(mod_raw)[-1,]
# pR2(mod_raw)
# visreg(mod_raw,"ptd006")
# visreg(mod_raw,"ptd003")
# visreg(mod_raw,"ptd0045")

cat("\n\n###########################################")
print("Trauma")
###########################################
#Lasso and ridge with CV
###########################################

#amelia data set
x_total = merge(Y_bucket,Trauma_bucket_amelia[,-9])

# #original data set
# x_total = merge(Y_bucket,Trauma_bucket[,-9])
# #remove rows with NA 
# x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)
run_ridge(x,y)

##########################################
# relieff (according to P_value)
##########################################
run_stir(x,y,2)

##########################################
# Random Forest 
##########################################
run_tree_RF(x,y,2)

###########################################
#Check Trauma weird results 
###########################################
#make sure Demographics bucket is ready 

# #1. calculate AUC for Demographics 
# x = merge(Y_bucket,Demographics_bucket)
# demo_b = Demographics_bucket[,-1]
# 
# resids = create_resids(demo_b)
# x <- data.frame(x,resids)
# 
# set.seed(42)
# mod_raw <- glm(Lifetime_Suicide_Attempt~sex+ ethnicity + tml007 + age + goassessPhqDurMonths +race2_White + race2_Black,data=x,family="binomial")
# summary(mod_raw)
# y_predicted <- predict(mod_raw, type="response")
# pred <- prediction(y_predicted, x$Lifetime_Suicide_Attempt)
# #calculate AUC
# performance(pred, measure = "auc")@y.values[[1]] #0.6518222
# 
# 
# #resid
# mod_resid <- glm(Lifetime_Suicide_Attempt~goassessPhqDurMonths_res + sex_res + ethnicity_res + tml007_res +
#                   age_res + race2_White_res + race2_Black_res ,data=x,family="binomial")
# summary(mod_resid)
# y_predicted <- predict(mod_resid, type="response")
# pred <- prediction(y_predicted, x$Lifetime_Suicide_Attempt)
# #calculate AUC
# performance(pred, measure = "auc")@y.values[[1]] #0.6518222
# 
# 
# #2. add all trauma
# x = merge(x,Trauma_bucket_amelia)
# 
# mod_raw <- glm(Lifetime_Suicide_Attempt~sex+ ethnicity + tml007 + age + goassessPhqDurMonths +race2_White + race2_Black+ 
#                  ptd001+ptd002+ptd003+ptd0045+ptd006+ptd007+ptd008+ptd009,data=x,family="binomial")
# summary(mod_raw)
# 
# y_predicted <- predict(mod_raw, type="response")
# pred <- prediction(y_predicted, x$Lifetime_Suicide_Attempt)
# 
# #calculate AUC
# performance(pred, measure = "auc")@y.values[[1]] #0.6839159
# 
# 
# 
# #3. add each feature separately and compare to Lifetime_Suicide_Attempt
# 
# names_trauma = colnames(Trauma_bucket[,-1])[-8]
# 
# for(name in names_trauma){
# 
#   mod_raw <- glm(Lifetime_Suicide_Attempt~sex+ ethnicity + tml007 + age + goassessPhqDurMonths +race2_White + race2_Black+ 
#                  x[,name] ,data=x,family="binomial")
#   summary(mod_raw)
#   
#   y_predicted <- predict(mod_raw, type="response")
#   pred <- prediction(y_predicted, x$Lifetime_Suicide_Attempt)
#   
#   auc = round(performance(pred, measure = "auc")@y.values[[1]], digits = 3) 
#   cor = round(polychoric(data.frame(x[,name],x$Lifetime_Suicide_Attempt))$rho[2],digits = 5)
#   
#   cat("\n AUC Demographics + ", name, ": ", auc, " polychoric(Lifetime_Suicide_Attempt,",name,")$rho: " ,cor)
# }
# 
# #3. add each feature separately and compare to Lifetime_Suicide_Attempt
# 
# names_trauma = colnames(Trauma_bucket[,-1])[-8]
# x_above11 = x[x$above11==1,]
# 
# for(name in names_trauma){
#   
#   mod_raw <- glm(Lifetime_Suicide_Attempt~sex+ ethnicity + tml007 + age + goassessPhqDurMonths +race2_White + race2_Black+ 
#                    x_above11[,name] ,data=x_above11,family="binomial")
#   summary(mod_raw)
#   
#   y_predicted <- predict(mod_raw, type="response")
#   pred <- prediction(y_predicted, x_above11$Lifetime_Suicide_Attempt)
#   
#   auc = round(performance(pred, measure = "auc")@y.values[[1]], digits = 3) 
#   cor = round(polychoric(data.frame(x_above11[,name],x_above11$Lifetime_Suicide_Attempt))$rho[2],digits = 5)
#   
#   cat("\n AUC Demographics + ", name, ": ", auc, " polychoric(Lifetime_Suicide_Attempt,",name,")$rho: " ,cor)
# }
# 
# # for ptd002 in polychoric:
# # Warning message:
# # In matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy) :
# # 1 cells were adjusted for 0 values using the correction for continuity. Examine your data carefully. 
# 
