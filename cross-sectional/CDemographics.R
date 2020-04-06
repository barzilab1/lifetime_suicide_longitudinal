library(PerformanceAnalytics)
library(psych)


summary(Demographics_bucket_cross[,-1]) 
describe(Demographics_bucket_cross[,-1])


#missing cnb age will be the goassess1 age
Demographics_bucket_cross$ageAtCnb1[is.na(Demographics_bucket_cross$ageAtCnb1)] = 
  Demographics_bucket_cross$ageAtClinicalAssess1[is.na(Demographics_bucket_cross$ageAtCnb1)]

#get mean of age at goassess1 and cnb ( we don't need both)
Demographics_bucket_cross$age = rowMeans(Demographics_bucket_cross[,c("ageAtClinicalAssess1", "ageAtCnb1")])

#remove the other age features
Demographics_bucket_cross = subset(Demographics_bucket_cross, select=-c(ageAtClinicalAssess1,ageAtCnb1))


#create 2 variables for race2 
Demographics_bucket_cross$race2_White = ifelse(Demographics_bucket_cross$race2 == 1 , 1, 0)
Demographics_bucket_cross$race2_Black = ifelse(Demographics_bucket_cross$race2 == 2 , 1, 0)
#remove race2
Demographics_bucket_cross = subset(Demographics_bucket_cross, select=-c(race2))

#change sex and ethnicity range from [1,2] to [0,1]
Demographics_bucket_cross[,c("sex","ethnicity")] =  Demographics_bucket_cross[,c("sex","ethnicity")] -1


summary(Demographics_bucket_cross[,-1]) 
chart.Correlation(Demographics_bucket_cross[,-1])
describe(Demographics_bucket_cross[,-1])
boxplot(Demographics_bucket_cross[,-1])


####TODO amelia - can we use edu1? 



#Frequency
sum(Demographics_bucket_cross$sex)/nrow(Demographics_bucket_cross) #0.54
sum(Demographics_bucket_cross$ethnicity)/nrow(Demographics_bucket_cross) #0.94
sum(Demographics_bucket_cross$race2_White)/nrow(Demographics_bucket_cross) #0.57
sum(Demographics_bucket_cross$race2_Black)/nrow(Demographics_bucket_cross) #0.32

#######################################
#Logistic regression 
#######################################

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
temp_data = Demographics_bucket_cross

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


###########################################
#Lasso and ridge with CV 
###########################################

#original data
x_total = merge(Y_bucket,Demographics_bucket_cross)

y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)
run_ridge(x,y)
##########################################
# relieff (according to P_value)
##########################################
run_stir(x,y,2)


