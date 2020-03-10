library(PerformanceAnalytics)
library(psych)
library(pscl)

summary(Environment_bucket[,-1])

#remove neighborhoodCrime. 542 out of 922 (59%) are missing
Environment_bucket = Environment_bucket[,! names(Environment_bucket) %in% c("neighborhoodCrime")]

# MedianFamilyIncome <0 , 556 620 => whill be removed as outliers
# Environment_bucket = Environment_bucket[Environment_bucket$MedianFamilyIncome >= 0,]

describe(Environment_bucket[,-1])
chart.Correlation(Environment_bucket[,-1])
boxplot(Environment_bucket[,-1])

#shift outliers values
Environment_bucket_trimmed = winsor(Environment_bucket[,-1],trim=0.005)
Environment_bucket_trimmed = data.frame(Environment_bucket$bblid, Environment_bucket_trimmed)
colnames(Environment_bucket_trimmed)[1] <- "bblid"

describe(Environment_bucket_trimmed[,-1])
summary(Environment_bucket_trimmed[,-1])
boxplot(Environment_bucket_trimmed[,-1])
chart.Correlation(Environment_bucket_trimmed[,-1])


#scale 
Environment_bucket_scaled = Environment_bucket
Environment_bucket_scaled[,-1] = scale(Environment_bucket[,-1])
summary(Environment_bucket_scaled)

# Environment_bucket_trimmed_scaled = data.frame(Environment_bucket[,c(1,5)], scale(Environment_bucket_trimmed[,-c(1,5)]))
Environment_bucket_trimmed_scaled = Environment_bucket_trimmed
Environment_bucket_trimmed_scaled[,-1] = scale(Environment_bucket_trimmed[,-1])
summary(Environment_bucket_trimmed_scaled)

#######################################
#Logistic regression 
#######################################

#trimmed data
x = merge(Y_bucket,Environment_bucket_trimmed_scaled)
enviro_b = Environment_bucket_trimmed_scaled[,-1]

#original data
x = merge(Y_bucket,Environment_bucket_scaled)
enviro_b = Environment_bucket_scaled[,-1]


resids = create_resids(enviro_b)

# add residual columns to data frame
x <- data.frame(x,resids)


### Lifetime_Suicide_Attempt
set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(enviro_b),data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)

mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


####when running only with MedianFamilyIncome also negative 
# x= merge(Y_bucket,Environment_bucket)
# mod_raw <- glm(Lifetime_Suicide_Attempt~MedianFamilyIncome,data=x,family="binomial")
# summary(mod_raw)


# Run with the opposit (multiply with -1) of all negative features 

#trimmed data
temp_data = Environment_bucket_trimmed

#origianl data
temp_data = Environment_bucket


#regressed regression
temp_data$PWhite = temp_data$PWhite * -1
temp_data$PercentMarried = temp_data$PercentMarried * -1
temp_data$MedianFamilyIncome = temp_data$MedianFamilyIncome * -1
temp_data$PercentHighSchoolPlus = temp_data$PercentHighSchoolPlus * -1
temp_data$MED_AGE = temp_data$MED_AGE * -1
summary(temp_data)

#scale 
temp_data[,-1] = scale(temp_data[,-1])

x = merge(Y_bucket,temp_data)
enviro_b = temp_data[,-1]

resids = create_resids(enviro_b)

# add residual columns to data frame
x <- data.frame(x,resids)

#regressed regression
set.seed(42)
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


###########################################
#Lasso and ridge with  CV 
###########################################

#trimmed data
x_total = merge(Y_bucket,Environment_bucket_trimmed)

#original data
# x_total = merge(Y_bucket,Environment_bucket)


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)
run_ridge(x,y)


##########################################
#features selection according to the lasso
##########################################

####Lifetime_Suicide_Attempt 
### as there is no clear "knee" in the graph, select according to the avg. 

#trimmed data
x = merge(Y_bucket,Environment_bucket_trimmed[,c("bblid","MedianFamilyIncome","PWhite", 
                                                 "PercentHighSchoolPlus")])
enviro_b = Environment_bucket_trimmed[,c("MedianFamilyIncome","PWhite", "PercentHighSchoolPlus")]


#original data
# x = merge(Y_bucket,Environment_bucket_scaled[,c("bblid")])
# enviro_b = Environment_bucket_scaled[,c()]

resids = create_resids(enviro_b)

set.seed(42)
mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(enviro_b),data=x,family="binomial")
summary(mod_raw)
get_logistic_results(mod_raw)[-1,]
pR2(mod_raw)

mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


# Run with the opposit (multiply with -1) of all negative features 

#trimmed data
temp_data = Environment_bucket_trimmed[,c("bblid","MedianFamilyIncome","PWhite", "PercentHighSchoolPlus")]

#origianl data
# temp_data = Environment_bucket[,c("bblid")]

#regressed regression
temp_data[,-1] = temp_data[,-1] *-1


#scale 
temp_data[,-1] = scale(temp_data[,-1])

x = merge(Y_bucket,temp_data)
enviro_b = temp_data[,-1]

resids = create_resids(enviro_b)

# add residual columns to data frame
x <- data.frame(x,resids)

#regressed regression
mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
summary(mod_resid)
get_logistic_results(mod_resid)[-1,]
pR2(mod_resid)


