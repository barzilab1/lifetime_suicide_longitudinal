
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
describe(Demographics_bucket_cross[,-1])
boxplot(Demographics_bucket_cross[,-1])

boxplot(Demographics_bucket_cross$tml007)
# remove outlier in tml007 (last class in school)
# TODO - is this the best why? should we only remove the extrime values? 
Demographics_bucket_cross$tml007 = winsor(Demographics_bucket_cross$tml007,trim=0.005)
boxplot(Demographics_bucket_cross$tml007)

set.seed(24)
amelia_fit <- amelia(Demographics_bucket_cross,m=1, idvars=c("bblid"), ords = c(2:5,7:8))
summary(amelia_fit)

Demographics_cross_amelia = amelia_fit$imputations[[1]]
describe(Demographics_cross_amelia[,-1])
summary(Demographics_cross_amelia)

#remove edu
Demographics_cross_amelia = subset(Demographics_cross_amelia, select= -c(edu1))
Demographics_bucket_cross = subset(Demographics_bucket_cross, select= -c(edu1))


#Frequency
sum(Demographics_bucket_cross$sex)/nrow(Demographics_bucket_cross) #0.54
sum(Demographics_bucket_cross$ethnicity)/nrow(Demographics_bucket_cross) #0.94
sum(Demographics_bucket_cross$race2_White)/nrow(Demographics_bucket_cross) #0.57
sum(Demographics_bucket_cross$race2_Black)/nrow(Demographics_bucket_cross) #0.32

cat("\n\n###########################################Demographics_cross")

x_total = merge(Y_bucket_cross,Demographics_cross_amelia)

if(!imputation){
  #original data set
  x_total = merge(Y_bucket_cross,Demographics_bucket_cross)
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
