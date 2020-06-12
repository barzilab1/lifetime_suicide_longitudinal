
summary(Environment_bucket[,-1])

#remove neighborhoodCrime. 542 out of 922 (59%) are missing
Environment_bucket = Environment_bucket[,! names(Environment_bucket) %in% c("neighborhoodCrime")]

# MedianFamilyIncome <0 , 556 620 => whill be removed as outliers
describe(Environment_bucket[,-1])
# chart.Correlation(Environment_bucket[,-1])
boxplot(Environment_bucket[,-1])

#shift outliers values
Environment_bucket_trimmed = winsor(Environment_bucket[,-1],trim=0.005)
Environment_bucket_trimmed = data.frame(Environment_bucket$bblid, Environment_bucket_trimmed)
colnames(Environment_bucket_trimmed)[1] <- "bblid"

describe(Environment_bucket_trimmed[,-1])
summary(Environment_bucket_trimmed[,-1])
boxplot(Environment_bucket_trimmed[,-1])
# chart.Correlation(Environment_bucket_trimmed[,-1])

environment_names = names(Environment_bucket)[-1]


cat("\n\n###########################################")
print("Environment")

#trimmed data
x_total = merge(Y_bucket,Environment_bucket_trimmed)

#original data
# x_total = merge(Y_bucket,Environment_bucket)


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

###########################################
#Lasso and ridge with CV 
###########################################
# run_lasso(x,y[,2])
run_ridge(x,y[,2])

##########################################
# relieff (according to P_value)
##########################################
# run_stir(x,y[,2])

##########################################
# Random Forest 
##########################################
run_tree_RF(x,y[,2])

