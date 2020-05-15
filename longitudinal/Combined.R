###########################################
#all features
###########################################

Clinical_bucket2 = Clinical_bucket[,! names(Clinical_bucket) %in% c("above11")]
Trauma_bucket2 = Trauma_bucket[,! names(Trauma_bucket) %in% c("above11")]


combined_bucket = Reduce(function(x, y) merge(x, y, by="bblid", all.x=TRUE), list(Environment_bucket_trimmed,
                                                                                  Demographics_bucket,
                                                                                  Family_bucket,
                                                                                  Trauma_bucket2,
                                                                                  Clinical_bucket2,
                                                                                  Cognitive_bucket))





#make sure all columns have less than 10% NA
which(colSums(is.na(combined_bucket))>=90) #0   

#check empty rows
which(rowSums(is.na(combined_bucket))>=100) #36  99 905
sum(is.na(combined_bucket[36,])) #124
sum(is.na(combined_bucket[99,])) #131
sum(is.na(combined_bucket[905,]))#157


set.seed(42)
amelia_fit <- amelia(combined_bucket ,m=1,  idvars=c("bblid"), ords = c(16:19, 21:22,  #demographics
                                                                        23:27, 29,     #family
                                                                        30:37,         #trauma
                                                                        38:168         #clinical
                                                                        ))

summary(amelia_fit)

combined_bucket_amelia = amelia_fit$imputations[[1]]
summary(combined_bucket_amelia[,-1])


cat("\n\n###########################################")
print("Combined")

x_total = merge(Y_bucket, combined_bucket_amelia)

y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]


###########################################
#Lasso and ridge with CV 
###########################################
res_lasso = run_lasso(x,y[,2])
run_ridge(x,y[,2])

##########################################
# relieff (according to P_value)
##########################################
res_Relieff = run_stir(x,y[,2])

##########################################
# Random Forest 
##########################################
res_rf = run_tree_RF(x,y[,2])


