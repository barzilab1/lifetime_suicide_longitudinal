
summary(Environment_bucket_cross[,-1])

#remove neighborhoodCrime. (72%) are missing
Environment_bucket_cross = Environment_bucket_cross[,! names(Environment_bucket_cross) %in% c("neighborhoodCrime")]

# MedianFamilyIncome <0 , 1692 2804 4448 => whill be removed as outliers
# PercentInPoverty < 0, 652
# PWhite == 1.16? 
describe(Environment_bucket_cross[,-1])
# chart.Correlation(Environment_bucket_cross[,-1])
boxplot(Environment_bucket_cross[,-c(1,5,7,10,14)])

#shift outliers values
Environment_cross_trim = winsor(Environment_bucket_cross[,-1],trim=0.005)
Environment_cross_trim = data.frame(Environment_bucket_cross$bblid, Environment_cross_trim)
colnames(Environment_cross_trim)[1] <- "bblid"

describe(Environment_cross_trim[,-1])
summary(Environment_cross_trim[,-1])
boxplot(Environment_cross_trim[,-1])
# chart.Correlation(Environment_cross_trim[,-1])

cat("\n\n###########################################Environment_cross")

#trimmed data
x_total = merge(Y_bucket_cross,Environment_cross_trim)

#original data
# x_total = merge(Y_bucket,Environment_bucket)


y = x_total[, c(2)]
x = x_total[,-c(1:2)]

###########################################
# ridge with CV 
###########################################
run_ridge(x,y)
##########################################
# Random Forest 
##########################################
run_tree_RF(x,y)
