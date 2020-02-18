library(PerformanceAnalytics)
library(psych)


summary(Demographics_bucket[,-1]) 
describe(Demographics_bucket[,-1])


#add the phq age
Demographics_bucket$ageAtPHQAssess2 = Demographics_bucket$ageAtClinicalAssess1 + Demographics_bucket$goassessPhqDurMonths
#remove goassessPhqDurMonths
Demographics_bucket = subset(Demographics_bucket, select=-c(goassessPhqDurMonths))


#missing cnb age will be the goassess1 age
Demographics_bucket$ageAtCnb1[is.na(Demographics_bucket$ageAtCnb1)] = 
  Demographics_bucket$ageAtClinicalAssess1[is.na(Demographics_bucket$ageAtCnb1)]

#get mean of age at goassess1 and cnb ( we don't need both)
Demographics_bucket$age = rowMeans(data.frame(Demographics_bucket$ageAtClinicalAssess1, 
                                              Demographics_bucket$ageAtCnb1))

#remove the other age features
Demographics_bucket = subset(Demographics_bucket, select=-c(ageAtClinicalAssess1,ageAtCnb1))


#create 2 variables for race2 
Demographics_bucket$race2_White = ifelse(Demographics_bucket$race2 == 1 , 1, 0)
Demographics_bucket$race2_Black = ifelse(Demographics_bucket$race2 == 2 , 1, 0)
#remove race2
Demographics_bucket = subset(Demographics_bucket, select=-c(race2))

#change sex and ethnicity range from [1,2] to [0,1]
Demographics_bucket[,c("sex","ethnicity")] =  Demographics_bucket[,c("sex","ethnicity")] -1

t = floor(Demographics_bucket$age/12) - Demographics_bucket$tml007
summary(t) #range [4,7]
t = Demographics_bucket$edu1 - Demographics_bucket$tml007
summary(t) #range [-2,1]
t = floor(Demographics_bucket$age/12) - Demographics_bucket$edu1
summary(t) #range [5,8]

# remove edu1
Demographics_bucket = subset(Demographics_bucket, select=-c(edu1))


summary(Demographics_bucket[,-1]) 
chart.Correlation(Demographics_bucket[,-1])
describe(Demographics_bucket[,-1])
boxplot(Demographics_bucket[,-1])


#scale only the non-binary features 
Demographics_bucket_scaled = Demographics_bucket
Demographics_bucket_scaled[,4:6] = scale(Demographics_bucket[,4:6]) 
describe(Demographics_bucket_scaled[,-1])

#Frequency
sum(Demographics_bucket$sex)/nrow(Demographics_bucket) #0.476
sum(Demographics_bucket$ethnicity)/nrow(Demographics_bucket) #0.937
sum(Demographics_bucket$race2_White)/nrow(Demographics_bucket) #0.400
sum(Demographics_bucket$race2_Black)/nrow(Demographics_bucket) #0.487

#######################################
#Logistic regression 
x = merge(Y_bucket,Demographics_bucket_scaled)
demo_b = Demographics_bucket_scaled[,-1]

resids = create_resids(demo_b)

# add residual columns to data frame
x <- data.frame(x,resids)

### Lifetime_Suicide_Attempt
set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(demo_b),data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)

mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


# Run with the opposit (multiply with -1) of all negative features
temp_data = Demographics_bucket

#regressed regression data
temp_data$race2_White = temp_data$race2_White * -1
temp_data$race2_White = temp_data$race2_White + 1

#scale 
temp_data[,4:6] = scale(temp_data[,4:6]) 

x = merge(Y_bucket,temp_data)
demo_b = temp_data[,-1]

resids = create_resids(demo_b)

# add residual columns to data frame
x <- data.frame(x,resids)

#regressed regression
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


### Current_Suicidal_Ideation
# set.seed(42)
# mod_raw <- glm(Current_Suicidal_Ideation~as.matrix(demo_b),data=x,family="binomial")
# mod_resid <- glm(Current_Suicidal_Ideation~resids,data=x,family="binomial")
# summary(mod_raw)
# summary(mod_resid)
# pR2(mod_resid)
# 
# set.seed(42)
# mod_raw <- glm(Depression_mod_above_at_phq~as.matrix(demo_b),data=x,family="binomial")
# mod_resid <- glm(Depression_mod_above_at_phq~resids,data=x,family="binomial")
# summary(mod_raw)
# summary(mod_resid)
# pR2(mod_resid)


###########################################
#Lasso with  CV

#original data
x_total = merge(Y_bucket,Demographics_bucket)

y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)

##########################################
#features selection according to the lasso

####Lifetime_Suicide_Attempt 
### the "knee" in the graph equal to the avg. 

x = merge(Y_bucket,Demographics_bucket_scaled[,c("bblid","sex", "race2_White")])
demo_b = Demographics_bucket_scaled[,c("sex", "race2_White")]

resids = create_resids(demo_b)

# add residual columns to data frame
x <- data.frame(x,resids)

set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(demo_b),data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)

mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


# Run with the opposit (multiply with -1) of all negative features
temp_data = Demographics_bucket[,c("bblid","sex", "race2_White")]

#regressed regression data
temp_data$race2_White = temp_data$race2_White * -1
temp_data$race2_White = temp_data$race2_White + 1

#scale 
temp_data[,4:6] = scale(temp_data[,4:6]) 

x = merge(Y_bucket,temp_data)
demo_b = temp_data[,-1]

resids = create_resids(demo_b)

# add residual columns to data frame
x <- data.frame(x,resids)

set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(demo_b),data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)

mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


###########################################
#ridge with  CV 

#original data
x_total = merge(Y_bucket,Demographics_bucket)


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_ridge(x,y)



