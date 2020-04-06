library(Amelia)
library(PerformanceAnalytics)
library(parcor)
library(psych)
library(qgraph)

# [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
summary(Clinical_bucket_cross[,-1])
# describe(Clinical_bucket[,-1])

# 37 rows missing all the medical variables 
t = Clinical_bucket_cross[,c(118:125)]
nrow(t[rowSums(is.na(t)) == 8,])


#how many repeat a grade
sum(Clinical_bucket_cross$dem107, na.rm = TRUE) #986

#TODO should we handle outliers for gaf? No
boxplot(Clinical_bucket_cross[,c(117)])

############
#Substance
summary(Substance_bucket_cross)

#convert charecter to numeric
Substance_bucket_cross$AlcUse = ifelse(Substance_bucket_cross$AlcUse == "nonuser" , 0 ,1) 


summary(Substance_bucket_cross)

#merge the tables, take all kids from clinical
Clinical_bucket_cross = merge(x=Clinical_bucket_cross,y=Substance_bucket_cross,all.x = TRUE)

summary(Clinical_bucket_cross)

#get cor including Y
write.csv(cor_auto(merge(Clinical_bucket_cross, Y_bucket_cross)) ,file = "cor_clinic_cross.csv")



set.seed(42)
amelia_fit <- amelia(Clinical_bucket_cross ,m=1,  idvars=c("bblid"), ords = c(2:132))

summary(amelia_fit)

Clinical_bucket_c_am = amelia_fit$imputations[[1]]
describe(Clinical_bucket_c_am[,-1])
summary(Clinical_bucket_c_am[,-1])


###########################################
#Lasso and ridge with CV
##########################################

#amelia data set
x_total = merge(Y_bucket,Clinical_bucket_amelia)

#original data set
x_total = merge(Y_bucket,Clinical_bucket)
#remove empty tows 
x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]



x_total = x_total[,! names(x_total) %in% c("above11")]

y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y,2)
run_ridge(x,y)

##########################################
# relieff (according to P_value)
##########################################
run_stir(x,y,2)






 