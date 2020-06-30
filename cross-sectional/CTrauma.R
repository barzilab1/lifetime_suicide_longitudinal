
summary(Trauma_bucket_cross)

#sexual assault or attack
table(Trauma_bucket_cross$ptd004, Trauma_bucket_cross$ptd005, dnn = c("4","5")) #2 items are in conflict
# info is miising in ptd005
which(Trauma_bucket_cross$ptd004 == 1 & is.na(Trauma_bucket_cross$ptd005) ) # 3 items

#create new var (4 or 5) and remove ptd004 & ptd005
Trauma_bucket_cross$ptd0045 = Trauma_bucket_cross$ptd004
Trauma_bucket_cross$ptd0045[Trauma_bucket_cross$ptd005 == 1] = 1 
Trauma_bucket_cross = Trauma_bucket_cross[,! names(Trauma_bucket_cross) %in% c("ptd004","ptd005")]

summary(Trauma_bucket_cross)
describe(Trauma_bucket_cross)
chart.Correlation(Trauma_bucket_cross[,-1])
boxplot(Trauma_bucket_cross[,-1])

#remove empty rows
length(which(rowSums(is.na(Trauma_bucket_cross)) >= 8)) #30
Trauma_bucket_cross = Trauma_bucket_cross[!(rowSums(is.na(Trauma_bucket_cross)) >= 8),]


set.seed(24)
amelia_fit <- amelia(Trauma_bucket_cross,m=1, idvars=c("bblid"), ords = c(2:9))

summary(amelia_fit)

Trauma_cross_amelia = amelia_fit$imputations[[1]]
describe(Trauma_cross_amelia)
summary(Trauma_cross_amelia)

#Frequency
sum(Trauma_bucket_cross$ptd001, na.rm = TRUE)/nrow(Y_bucket_cross) #0.040
sum(Trauma_bucket_cross$ptd002, na.rm = TRUE)/nrow(Y_bucket_cross) #0.132
sum(Trauma_bucket_cross$ptd003, na.rm = TRUE)/nrow(Y_bucket_cross) #0.070
sum(Trauma_bucket_cross$ptd0045, na.rm = TRUE)/nrow(Y_bucket_cross) #0.040
sum(Trauma_bucket_cross$ptd006, na.rm = TRUE)/nrow(Y_bucket_cross) #0.063
sum(Trauma_bucket_cross$ptd007, na.rm = TRUE)/nrow(Y_bucket_cross) #0.117
sum(Trauma_bucket_cross$ptd008, na.rm = TRUE)/nrow(Y_bucket_cross) #0.219
sum(Trauma_bucket_cross$ptd009, na.rm = TRUE)/nrow(Y_bucket_cross) #0.275


cat("\n\n###########################################Trauma_cross")

#amelia data set
x_total = merge(Y_bucket_cross,Trauma_cross_amelia)

if(no_amelia){
  #original data set
  x_total = merge(Y_bucket_cross,Trauma_bucket_cross)
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
