summary(Trauma_bucket)

#sexual assault or attack
table(Trauma_bucket$ptd004, Trauma_bucket$ptd005) #no conflicts
# info is miising in ptd005
which(Trauma_bucket$ptd004 == 1 & is.na(Trauma_bucket$ptd005) ) # [641]

#create new var (4 or 5) and remove ptd004 & ptd005
Trauma_bucket$ptd0045 = Trauma_bucket$ptd004
Trauma_bucket$ptd0045[Trauma_bucket$ptd005 == 1] = 1 
Trauma_bucket = Trauma_bucket[,! names(Trauma_bucket) %in% c("ptd004","ptd005")]

summary(Trauma_bucket)
boxplot(Trauma_bucket[,-1])
describe(Trauma_bucket[,-1])

#remove empty rows
length(which(rowSums(is.na(Trauma_bucket)) >= 8)) #5
Trauma_bucket = Trauma_bucket[!(rowSums(is.na(Trauma_bucket)) >= 8),]

#no need to scale as all features are binary

#Frequency
sum(Trauma_bucket$ptd001, na.rm = TRUE)/nrow(Y_bucket) #0.041
sum(Trauma_bucket$ptd002, na.rm = TRUE)/nrow(Y_bucket) #0.079
sum(Trauma_bucket$ptd003, na.rm = TRUE)/nrow(Y_bucket) #0.026
sum(Trauma_bucket$ptd0045, na.rm = TRUE)/nrow(Y_bucket) #0.016
sum(Trauma_bucket$ptd006, na.rm = TRUE)/nrow(Y_bucket) #0.015
sum(Trauma_bucket$ptd007, na.rm = TRUE)/nrow(Y_bucket) #0.059
sum(Trauma_bucket$ptd008, na.rm = TRUE)/nrow(Y_bucket) #0.101
sum(Trauma_bucket$ptd009, na.rm = TRUE)/nrow(Y_bucket) #0.118


Trauma_bucket