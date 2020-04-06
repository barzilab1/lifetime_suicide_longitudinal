library(PerformanceAnalytics)
library(psych)
library(pscl)

summary(Environment_bucket_cross[,-1])

#remove neighborhoodCrime. (72%) are missing
Environment_bucket_cross = Environment_bucket_cross[,! names(Environment_bucket_cross) %in% c("neighborhoodCrime")]

# MedianFamilyIncome <0 , 1692 2804 4448 => whill be removed as outliers
# PercentInPoverty < 0, 652
# PWhite == 1.16? 
describe(Environment_bucket_cross[,-1])
chart.Correlation(Environment_bucket_cross[,-1])
boxplot(Environment_bucket_cross[,-1])

#shift outliers values
Environment_bucket_c_t = winsor(Environment_bucket_cross[,-1],trim=0.005)
Environment_bucket_c_t = data.frame(Environment_bucket_cross$bblid, Environment_bucket_c_t)
colnames(Environment_bucket_c_t)[1] <- "bblid"

describe(Environment_bucket_c_t[,-1])
summary(Environment_bucket_c_t[,-1])
boxplot(Environment_bucket_c_t[,-1])
chart.Correlation(Environment_bucket_c_t[,-1])


###########################################
#Lasso and ridge with CV 
###########################################
#trimmed data
x_total = merge(Y_bucket,Environment_bucket_c_t)

#original data
# x_total = merge(Y_bucket,Environment_bucket)


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

#trimmed data
x = merge(Y_bucket,Environment_bucket_c_t[,c("bblid","MedianFamilyIncome","PWhite", 
                                                 "PercentHighSchoolPlus")])
enviro_b = Environment_bucket_c_t[,c("MedianFamilyIncome","PWhite", "PercentHighSchoolPlus")]


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

