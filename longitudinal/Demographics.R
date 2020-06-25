summary(Demographics_bucket[,-1]) 
describe(Demographics_bucket[,-1])


#missing cnb age will be the goassess1 age
Demographics_bucket$ageAtCnb1[is.na(Demographics_bucket$ageAtCnb1)] = 
  Demographics_bucket$ageAtClinicalAssess1[is.na(Demographics_bucket$ageAtCnb1)]

#get mean of age at goassess1 and cnb ( we don't need both)
Demographics_bucket$age = rowMeans(Demographics_bucket[,c("ageAtClinicalAssess1", "ageAtCnb1")])

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
# chart.Correlation(Demographics_bucket[,-1])
describe(Demographics_bucket[,-1])
boxplot(Demographics_bucket[,-1])


#Frequency
sum(Demographics_bucket$sex)/nrow(Demographics_bucket) #0.476
sum(Demographics_bucket$ethnicity)/nrow(Demographics_bucket) #0.937
sum(Demographics_bucket$race2_White)/nrow(Demographics_bucket) #0.400
sum(Demographics_bucket$race2_Black)/nrow(Demographics_bucket) #0.487
sum(Demographics_bucket$ethnicity)/nrow(Demographics_bucket) #0.937


demographics_names = names(Demographics_bucket)[-1]

cat("\n\n###########################################Demographics")

#original data
x_total = merge(Y_bucket,Demographics_bucket)

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
