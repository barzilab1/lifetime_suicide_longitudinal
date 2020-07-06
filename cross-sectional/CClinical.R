

# [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
summary(Clinical_bucket_cross[,-1])
# describe(Clinical_bucket[,-1])

# 37 rows missing all the medical variables 
t = Clinical_bucket_cross[,c(118:125)]
nrow(t[rowSums(is.na(t)) == 8,])


#how many repeat a grade
sum(Clinical_bucket_cross$dem107, na.rm = TRUE) #986

#TODO should we handle outliers for gaf? No
boxplot(Clinical_bucket_cross$gaf001)
describe(Clinical_bucket_cross$gaf001)

############
#Substance
############
summary(Substance_bucket_cross)

#convert charecter to numeric
Substance_bucket_cross$AlcUse = ifelse(Substance_bucket_cross$AlcUse == "nonuser" , 0 ,1) 


summary(Substance_bucket_cross)

#merge the tables, take all kids from clinical
Clinical_bucket_cross = merge(x=Clinical_bucket_cross,y=Substance_bucket_cross,all.x = TRUE)

summary(Clinical_bucket_cross[,-1])

#get cor including Y
# write.csv(cor_auto(merge(Clinical_bucket_cross, Y_bucket_cross)) ,file = "cor_clinic_cross.csv")



set.seed(42)
amelia_fit <- amelia(Clinical_bucket_cross ,m=1,  idvars=c("bblid"), ords = c(2:131))

summary(amelia_fit)

Clinical_cross_amelia = amelia_fit$imputations[[1]]
describe(Clinical_cross_amelia[,-1])
summary(Clinical_cross_amelia[,-1])

cat("\n\n###########################################Clinical_cross")


#amelia data set
x_total = merge(Y_bucket_cross,Clinical_cross_amelia)

if(!imputation){
  #original data set
  x_total = merge(Y_bucket_cross,Clinical_bucket_cross)
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
