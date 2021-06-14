
summary(Family_bucket[,-1])
boxplot(Family_bucket[,-1])

#parents education
#check if there is an info that exists in each of the parents and not in the avg. 
which(!is.na(Family_bucket$medu1) & is.na(Family_bucket$AvgParentEducation)) #0
which(!is.na(Family_bucket$fedu1) & is.na(Family_bucket$AvgParentEducation)) #0
#remove parents education. (As avg. has less missing data)
Family_bucket = subset(Family_bucket, select=-c(medu1,fedu1 ))

describe(Family_bucket[,-1])

#Frequency
sum(Family_bucket$Ran_substance_FH, na.rm = TRUE)/nrow(Family_bucket) #0.1475
sum(Family_bucket$Ran_depression_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0965
sum(Family_bucket$Ran_bipolar_or_lithium_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0249
sum(Family_bucket$Ran_psychosis_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0293
sum(Family_bucket$Ran_Sui_attempt_or_death_FH, na.rm = TRUE)/nrow(Family_bucket) #0.0553
sum(Family_bucket$Parents_Sep_Divorce, na.rm = TRUE)/nrow(Family_bucket) #0.167


Family_bucket

