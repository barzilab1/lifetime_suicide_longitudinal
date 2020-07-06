

summary(Family_bucket_cross[,-1])
# chart.Correlation(Family_bucket_cross[,-1])

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

Family_cross_amelia = amelia_fit$imputations[[1]]
describe(Family_cross_amelia[,-1])
summary(Family_cross_amelia)


#Frequency
sum(Family_bucket_cross$Ran_substance_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.185
sum(Family_bucket_cross$Depression_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.11
sum(Family_bucket_cross$Bipolar_or_lithim_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.04
sum(Family_bucket_cross$Ran_psychosis_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.03
sum(Family_bucket_cross$Suicide_FH, na.rm = TRUE)/nrow(Family_bucket_cross) #0.07
sum(Family_bucket_cross$Parents_Sep_Divorce, na.rm = TRUE)/nrow(Family_bucket_cross) #0.2


cat("\n\n###########################################Family_cross")

#amelia data set
x_total = merge(Y_bucket_cross,Family_cross_amelia)

if(!imputation){
  #original data set
  x_total = merge(Y_bucket_cross,Family_bucket_cross)
  # remove rows with NA
  x_total = x_total[(rowSums(is.na(x_total)) == 0),]
}

cat("\nnumber of rows: ", nrow(x_total))

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
