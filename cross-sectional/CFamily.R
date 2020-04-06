library(Amelia)
library(PerformanceAnalytics)
library(parcor)


summary(Family_bucket_cross[,-1])
chart.Correlation(Family_bucket_cross[,-1])

boxplot(Family_bucket_cross[,-1])
# boxplot(winsor(Family_bucket[,-1],trim=0.005))

#check if there is an info that exists in each of the parents and not in the avg. 
which(!is.na(Family_bucket_cross$medu1) & is.na(Family_bucket_cross$AvgParentEducation)) #0
which(!is.na(Family_bucket_cross$fedu1) & is.na(Family_bucket_cross$AvgParentEducation)) #0
#remove parents education. (As avg. has less missing data)
Family_bucket_cross = subset(Family_bucket_cross, select=-c(medu1,fedu1 ))
describe(Family_bucket_cross[,-1])

set.seed(24)
#AvgParentEducation is not ord. should we set bounds? - only if we get odd results
amelia_fit <- amelia(Family_bucket_cross,m=1, idvars=c("bblid"), ords = c("Parents_Sep_Divorce",
                                                                    "Ran_substance_FH",
                                                                    "Bipolar_or_lithim_FH",
                                                                    "Ran_psychosis_FH",
                                                                    "Depression_FH",
                                                                    "Suicide_FH"))
summary(amelia_fit)

Family_bucket_c_am = amelia_fit$imputations[[1]]
describe(Family_bucket_c_am[,-1])
summary(Family_bucket_c_am)


#Frequency
sum(Family_bucket_cross$Ran_substance_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.185
sum(Family_bucket_cross$Depression_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.11
sum(Family_bucket_cross$Bipolar_or_lithim_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.04
sum(Family_bucket_cross$Ran_psychosis_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.03
sum(Family_bucket_cross$Suicide_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.07
sum(Family_bucket_cross$Parents_Sep_Divorce, na.rm = TRUE)/nrow(Family_bucket_cross) #0.2


###########################################
#Lasso and ridge with CV 
###########################################

#amelia data set
x_total = merge(Y_bucket,Family_bucket_c_am)

#original data set
x_total = merge(Y_bucket,Family_bucket_cross)
summary(x_total)
# remove rows with NA
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)
run_ridge(x,y)

##########################################
# relieff (according to P_value)
##########################################
run_stir(x,y,2)

##########################################
#features selection according to the lasso
##########################################
####Lifetime_Suicide_Attempt 
### as there is no clear "knee" in the graph, select according to the avg. 

#amelia data set
x = merge(Y_bucket,Family_bucket_amelia_scaled[,c("bblid","AvgParentEducation", "Ran_substance_FH", 
                                           "Parents_Sep_Divorce", "Ran_Sui_attempt_or_death_FH")])
fam_b = Family_bucket_amelia_scaled[,c("AvgParentEducation", "Ran_substance_FH", 
                                "Parents_Sep_Divorce", "Ran_Sui_attempt_or_death_FH")]

#original data set
x = merge(Y_bucket,Family_bucket_scaled[,c("bblid","AvgParentEducation", "Ran_substance_FH", 
                                                           "Parents_Sep_Divorce", "Ran_Sui_attempt_or_death_FH")])
fam_b = Family_bucket_scaled[,c("AvgParentEducation", "Ran_substance_FH", 
                                "Parents_Sep_Divorce", "Ran_Sui_attempt_or_death_FH")]

resids = create_resids(fam_b)

# add residual columns to data frame
x <- data.frame(x,resids)

#regular regression
set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(fam_b),data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)


#regressed regression
set.seed(42)
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)

##########################
# Run with the opposit (multiply with -1) of all negative features 

#amelia data

#regular regression
temp_data = Family_bucket_c_am[,c("bblid","AvgParentEducation", "Ran_substance_FH", 
                                    "Parents_Sep_Divorce", "Ran_Sui_attempt_or_death_FH")]
temp_data$Ran_substance_FH = temp_data$Ran_substance_FH * -1
temp_data$Ran_substance_FH = temp_data$Ran_substance_FH + 1
temp_data$AvgParentEducation = temp_data$AvgParentEducation * -1

#regressed regression
temp_data = Family_bucket_c_am[,c("bblid","AvgParentEducation", "Ran_substance_FH", 
                                    "Parents_Sep_Divorce", "Ran_Sui_attempt_or_death_FH")]
temp_data$AvgParentEducation = temp_data$AvgParentEducation * -1


#scale 
temp_data$AvgParentEducation = scale(temp_data$AvgParentEducation)
summary(temp_data)

x = merge(Y_bucket,temp_data)
fam_b = temp_data[,-1]


resids = create_resids(fam_b)

# add residual columns to data frame
x <- data.frame(x,resids)

#regular regression
set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(fam_b),data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)


#regressed regression
set.seed(42)
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)



