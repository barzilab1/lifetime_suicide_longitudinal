library(PerformanceAnalytics)
library(Amelia)


summary(Cognitive_bucket_cross[,-1])

#remove empty rows from origin cog
Cognitive_bucket_cross = Cognitive_bucket_cross[ !(rowSums(is.na(Cognitive_bucket_cross)) >= 26),]

describe(Cognitive_bucket_cross)

#remove bblid with battery_valid_collapsed == N
Cognitive_bucket_cross = merge(Cognitive_bucket_cross,PNC_Core_Data_cognitive_ALTERNATIVE[,c("bblid","battery_valid_collapsed")])
table(Cognitive_bucket_cross$battery_valid_collapsed) #N=13
Cognitive_bucket_cross = Cognitive_bucket_cross[Cognitive_bucket_cross$battery_valid_collapsed != "N",]
Cognitive_bucket_cross = Cognitive_bucket_cross[,! names(Cognitive_bucket_cross) %in% c("battery_valid_collapsed")]


###########################################
#Lasso and ridge with CV
###########################################

# #amelia data set
# x_total = merge(Y_bucket,Cognitive_bucket_amelia)
# 
# #original data set
# x_total = merge(Y_bucket,Cognitive_bucket_combined)
# #remove empty rows 
# x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]

#data without raw
x_total = merge(Y_bucket,Cognitive_bucket_cross)


y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)
run_ridge(x,y)
##########################################
# relieff (according to P_value)
##########################################
run_stir(x,y,2)




