

summary(Cognitive_bucket_cross[,-1])

#remove empty rows from cog
Cognitive_bucket_cross = Cognitive_bucket_cross[ !(rowSums(is.na(Cognitive_bucket_cross)) >= 26),]

describe(Cognitive_bucket_cross)

#remove bblid with battery_valid_collapsed == N
Cognitive_bucket_cross = merge(Cognitive_bucket_cross,PNC_Core_Data_cognitive_ALTERNATIVE[,c("bblid","battery_valid_collapsed")])
table(Cognitive_bucket_cross$battery_valid_collapsed) #N=13
Cognitive_bucket_cross = Cognitive_bucket_cross[Cognitive_bucket_cross$battery_valid_collapsed != "N",]
Cognitive_bucket_cross = Cognitive_bucket_cross[,! names(Cognitive_bucket_cross) %in% c("battery_valid_collapsed")]

summary(Cognitive_bucket_cross[,-1])



cat("\n\n###########################################Cognitive_cross")

#data without raw
x_total = merge(Y_bucket_cross,Cognitive_bucket_cross)


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
