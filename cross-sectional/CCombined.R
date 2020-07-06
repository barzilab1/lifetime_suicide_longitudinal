###########################################
#all 191 features
###########################################

combined_bucket = Reduce(function(x, y) merge(x, y, by="bblid", all.x=TRUE), list(Environment_cross_trim,
                                                                                  Demographics_bucket_cross,
                                                                                  Family_bucket_cross,
                                                                                  Trauma_bucket_cross,
                                                                                  Clinical_bucket_cross,
                                                                                  Cognitive_bucket_cross))





#make sure all columns have less than 10% NA
which(colSums(is.na(combined_bucket))>=667) #0   

#check empty rows
which(rowSums(is.na(combined_bucket))>=26) #0


#NA 
number_NA = sum(is.na(combined_bucket))
cat("\nNumber of Na in Data: ", number_NA, "\n")
number_NA_col = colSums(is.na(combined_bucket))
data_dim = dim(combined_bucket)
number_NA/((data_dim[2]-1)*data_dim[1]) #0.0034
cat("% of NA: ", number_NA/((data_dim[2]-1)*data_dim[1]),"\n" )

set.seed(42)
amelia_fit <- amelia(combined_bucket ,m=1,  idvars=c("bblid"), ords = c(16:18, 20:21,  #demographics
                                                                        22:26, 28,     #family
                                                                        29:36,         #trauma
                                                                        37:166         #clinical
                                                                        ))

summary(amelia_fit)

combined_bucket_amelia = amelia_fit$imputations[[1]]
summary(combined_bucket_amelia[,-1])


cat("\n\n###########################################Combined")

x_total = merge(Y_bucket_cross, combined_bucket_amelia)

if(!imputation){
  #original data set
  x_total = merge(Y_bucket_cross,combined_bucket)
  #remove rows with NA
  x_total = x_total[(rowSums(is.na(x_total)) == 0),]
}

cat("\nnumber of rows: ", nrow(x_total))


y = x_total[, c(2)]
x = x_total[,-c(1:2)]

###########################################
#Lasso and ridge with CV 
###########################################
res_lasso = run_lasso(x,y)
run_ridge(x,y)

##########################################
# relieff (according to P_value)
##########################################
res_Relieff = run_stir(x,y)

##########################################
# Random Forest 
##########################################
res_rf = run_tree_RF(x,y)


