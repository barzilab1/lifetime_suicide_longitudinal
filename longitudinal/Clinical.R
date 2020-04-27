
summary(Clinical_bucket[,-1])
# describe(Clinical_bucket[,-1])

# 3 rows have only the summary variables [medical vars that might be assessed by a different doc]  
t = Clinical_bucket[,c(2:118)]
nrow(t[rowSums(is.na(t)) == 117,])
# 4 rows missing all the summary variables [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
t = Clinical_bucket[,c(120:127)]
nrow(t[rowSums(is.na(t)) == 8,])

#remove ptd009.x=trauma
Clinical_bucket = Clinical_bucket[,! names(Clinical_bucket) %in% c("ptd009.x")]

#how many repeat a grade
sum(Clinical_bucket$dem107, na.rm = TRUE) #82

#TODO should we handle outliers for gaf? No
boxplot(Clinical_bucket[,c(118)])

############
#Substance
summary(Substance_bucket)

#total isn't a summary.  
t = GO1_Substance_Use[,c("subs_smry_sub_alc","subs_smry_sub_mar")]
t$remaining_sum = GO1_Substance_Use$subs_smry_sub_tot - rowSums(GO1_Substance_Use[,c(78:86)],na.rm = TRUE)
t$subs_smry_sub_mar[is.na(t$subs_smry_sub_mar)] = 0
lm(data=t[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc)),], remaining_sum ~ subs_smry_sub_mar )
which(t$subs_smry_sub_mar[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc))] != t$remaining_sum[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc))])

#check frequser of marichuana of the entire dataset - ok. 
table(GO1_Substance_Use$MarUse, GO1_Substance_Use$subs_smry_sub_mar)
table(Substance_bucket$MarUse, Substance_bucket$subs_smry_sub_mar) #total 4 users

#check frequser of alcohol of the entire dataset - not ok. stick with AlcUse 
table(GO1_Substance_Use$AlcUse, GO1_Substance_Use$subs_smry_sub_alc)
table(Substance_bucket$AlcUse, Substance_bucket$subs_smry_sub_alc)

#remove subs_smry_sub_alc and subs_smry_sub_tran (missing 66%) and subs_smry_sub_tot
Substance_bucket = Substance_bucket[,! names(Substance_bucket) %in% c("subs_smry_sub_alc", "subs_smry_sub_tran", "subs_smry_sub_tot")]

#convert charecter to numeric
Substance_bucket$MarUse = ifelse(Substance_bucket$MarUse == "nonuser" , 0 , 
                          ifelse(Substance_bucket$MarUse == "user", 1, 2))

Substance_bucket$AlcUse = ifelse(Substance_bucket$AlcUse == "nonuser" , 0 ,1) 

#take only substance with more 1% frequency  
Substance_bucket = Substance_bucket[,(apply(Substance_bucket, 2, sum, na.rm=TRUE) >=9)]


summary(Substance_bucket)

#merge the tables, take all kids from clinical
Clinical_bucket = merge(x=Clinical_bucket,y=Substance_bucket,all.x = TRUE)

summary(Clinical_bucket)

#get cor including Y
# write.csv(cor_auto(merge(Clinical_bucket,Y_bucket)) ,file = "cor_clinic.csv")



set.seed(42)
amelia_fit <- amelia(Clinical_bucket ,m=1,  idvars=c("bblid", "above11"), ords = c(2:127,129:133))

summary(amelia_fit)

Clinical_bucket_amelia = amelia_fit$imputations[[1]]
describe(Clinical_bucket_amelia[,-1])
summary(Clinical_bucket_amelia[,-1])

#scale only the non-binary features 
# Clinical_bucket_amelia_scaled = Clinical_bucket_amelia
# Clinical_bucket_amelia_scaled = scale(Clinical_bucket_amelia_scaled) 
# Clinical_bucket_scaled = Clinical_bucket
# Clinical_bucket_scaled = scale(Clinical_bucket_scaled) 



#######################################
#Logistic regression 
#######################################

#amelia data set
# x = 
# clinic_b = 

# #original data set
# x = merge(Y_bucket,Clinical_bucket)
# clinic_b = Clinical_bucket[,-1]
# 
# resids = create_resids(clinic_b)
# 
# # add residual columns to data frame
# x <- data.frame(x,resids)
# 
# ### Lifetime_Suicide_Attempt
# set.seed(42)
# mod_raw <- glm(Lifetime_Suicide_Attempt~as.matrix(clinic_b),data=x,family="binomial")
# summary(mod_raw)
# get_logistic_results(mod_raw)[-1,]
# pR2(mod_raw)
# 
# mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
# summary(mod_resid)
# get_logistic_results(mod_resid)[-1,]
# pR2(mod_resid)



cat("\n\n###########################################")
print("Clinical")
###########################################
#Lasso and ridge with CV
##########################################

#amelia data set
x_total = merge(Y_bucket,Clinical_bucket_amelia)

# #original data set
# x_total = merge(Y_bucket,Clinical_bucket)
# #remove empty tows 
# x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]


x_total = x_total[,! names(x_total) %in% c("above11")]

y = x_total[, c(2:5)]
x = x_total[,-c(1:5)]

run_lasso(x,y[,2])
run_ridge(x,y[,2])

##########################################
# relieff (according to P_value)
##########################################
run_stir(x,y[,2])

##########################################
# Random Forest 
##########################################
run_tree_RF(x,y[,2])




 