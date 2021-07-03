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
Demographics_bucket$race2_White = ifelse(Demographics_bucket$race2 == 1, 1, 0)
Demographics_bucket$race2_Black = ifelse(Demographics_bucket$race2 == 2 ,1, 0)
#remove race2
Demographics_bucket = subset(Demographics_bucket, select=-c(race2))

#change sex and ethnicity range from [1,2] to [0,1]
Demographics_bucket[,c("sex","ethnicity")] =  Demographics_bucket[,c("sex","ethnicity")] -1

#tml007: What is your grade in school? (or, if not in school, last completed grade)
temp = floor(Demographics_bucket$age/12) - Demographics_bucket$tml007
summary(temp)  #range [4,7]
temp = Demographics_bucket$edu1 - Demographics_bucket$tml007
summary(temp)  #range [-2,1]
temp = floor(Demographics_bucket$age/12) - Demographics_bucket$edu1
summary(temp)  #range [5,8]

# remove edu1
Demographics_bucket = subset(Demographics_bucket, select=-c(edu1))


summary(Demographics_bucket[,-1]) 
describe(Demographics_bucket[,-1])
boxplot(Demographics_bucket[,-1])


#Frequency
sum(Demographics_bucket$sex)/nrow(Demographics_bucket) #0.476
sum(Demographics_bucket$ethnicity)/nrow(Demographics_bucket) #0.937
sum(Demographics_bucket$race2_White)/nrow(Demographics_bucket) #0.400
sum(Demographics_bucket$race2_Black)/nrow(Demographics_bucket) #0.487

Demographics_bucket
