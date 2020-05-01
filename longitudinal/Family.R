
summary(Family_bucket[,-1])
# chart.Correlation(Family_bucket[,-1])

boxplot(Family_bucket[,-1])
# boxplot(winsor(Family_bucket[,-1],trim=0.005))

#check if there is an info that exists in each of the parents and not in the avg. 
which(!is.na(Family_bucket$medu1) & is.na(Family_bucket$AvgParentEducation)) #0
which(!is.na(Family_bucket$fedu1) & is.na(Family_bucket$AvgParentEducation)) #0
#remove parents education. (As avg. has less missing data)
Family_bucket = subset(Family_bucket, select=-c(medu1,fedu1 ))
describe(Family_bucket[,-1])

set.seed(24)
#AvgParentEducation is not ord. should we set bounds? - only if we get odd results
amelia_fit <- amelia(Family_bucket,m=1, idvars=c("bblid"), ords = c("Parents_Sep_Divorce",
                                                                    "Ran_substance_FH",
                                                                    "Ran_bipolar_or_lithium_FH",
                                                                    "Ran_psychosis_FH",
                                                                    "Ran_depression_FH",
                                                                    "Ran_Sui_attempt_or_death_FH"))
summary(amelia_fit)

Family_bucket_amelia = amelia_fit$imputations[[1]]
describe(Family_bucket_amelia[,-1])
summary(Family_bucket_amelia)

#scale only the non-binary features (AvgParentEducation)
Family_bucket_amelia_scaled = Family_bucket_amelia
Family_bucket_amelia_scaled$AvgParentEducation = scale(Family_bucket_amelia_scaled$AvgParentEducation) 
Family_bucket_scaled = Family_bucket
Family_bucket_scaled$AvgParentEducation = scale(Family_bucket_scaled$AvgParentEducation) 


#Frequency
sum(Family_bucket$Ran_substance_FH, na.rm = TRUE)/nrow(Family_bucket) #0.1475
sum(Family_bucket$Ran_depression_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0965
sum(Family_bucket$Ran_bipolar_or_lithium_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0249
sum(Family_bucket$Ran_psychosis_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0293
sum(Family_bucket$Ran_Sui_attempt_or_death_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0553
sum(Family_bucket$Parents_Sep_Divorce, na.rm = TRUE)/nrow(Family_bucket) #0.167

#######################################
#Logistic regression 
#######################################

# #amelia data set
# x = merge(Y_bucket,Family_bucket_amelia_scaled)
# fam_b = Family_bucket_amelia_scaled[,-1]
# 
# #original data set
# x = merge(Y_bucket,Family_bucket_scaled)
# fam_b = Family_bucket_scaled[,-1]
# 
# 
# resids = create_resids(fam_b)
# 
# # add residual columns to data frame
# x <- data.frame(x,resids)
# 
# ### Lifetime_Suicide_Attempt
# set.seed(42)
# mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(fam_b),data=x,family="binomial")
# summary(mod_raw)
# get_logistic_results(mod_raw)[-1,]
# pR2(mod_raw)
# 
# mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
# summary(mod_resid)
# get_logistic_results(mod_resid)[-1,]
# pR2(mod_resid)

cat("\n\n###########################################")
print("Family")
###########################################
#Lasso and ridge with CV 
###########################################

#amelia data set
x_total = merge(Y_bucket,Family_bucket_amelia)

# #original data set
# x_total = merge(Y_bucket,Family_bucket)
# summary(x_total)
# # remove rows with NA
# x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

# run_lasso(x,y[,2])
# run_ridge(x,y[,2])

##########################################
# relieff (according to P_value)
##########################################
# run_stir(x,y[,2])

##########################################
# Random Forest 
##########################################
# run_tree_RF(x,y[,2])


