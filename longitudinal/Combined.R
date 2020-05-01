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











###########################################
#lasso top selected features + age
###########################################
# combined_bucket_lasso_selected = Reduce(function(x, y) merge(x, y, by="bblid", all.x=TRUE), 
#                                                  list(Environment_bucket_trimmed[,c("bblid","MedianFamilyIncome","PercentHighSchoolPlus","PWhite")],
#                                                       Demographics_bucket[,c("bblid","sex","race2_White", "age", "goassessPhqDurMonths")],
#                                                       Family_bucket[,c("bblid","AvgParentEducation","Ran_substance_FH","Parents_Sep_Divorce","Ran_Sui_attempt_or_death_FH", "Ran_bipolar_or_lithium_FH")],
#                                                       Trauma_bucket[,c("bblid","ptd006","ptd0045")],
#                                                       Clinical_bucket[,c("bblid","sui002","man004","add016","add022","sip028","sip038","phb004","add012")],
#                                                       Cognitive_bucket[,c("bblid","vmem_s_ar_z","vmem_ar_z","abf_ar_z","fmem_s_ar_z","spa_ar_z")]))
# 
# set.seed(42)
# amelia_fit <- amelia(combined_bucket_lasso_selected ,m=1,  idvars=c("bblid"), 
#                      ords = c("sex","race2_White",
#                               "Ran_substance_FH","Parents_Sep_Divorce","Ran_Sui_attempt_or_death_FH","Ran_bipolar_or_lithium_FH",
#                               "ptd006","ptd0045",
#                               "sui002","man004","add016","add022","sip028","sip038","phb004","add012"
#                               ))
# 
# summary(amelia_fit)
# 
# combined_bucket_lasso_selected_amelia = amelia_fit$imputations[[1]]
# summary(combined_bucket_lasso_selected_amelia[,-1])
# 
# 
# 
# #lasso selected features - without sui002
# combined_bucket_lasso_nosui = combined_bucket_lasso_selected[,! names(Clinical_bucket) %in% c("sui002")]
# 
# set.seed(42)
# amelia_fit <- amelia(combined_bucket_lasso_nosui ,m=1,  idvars=c("bblid"), 
#                      ords = c("sex","race2_White",
#                               "Ran_substance_FH","Parents_Sep_Divorce","Ran_Sui_attempt_or_death_FH", "Ran_bipolar_or_lithium_FH",
#                               "ptd006","ptd0045",
#                               "man004","add016","add022","sip028","sip038","phb004","add012"
#                      ))
# 
# summary(amelia_fit)
# combined_bucket_lasso_nosui_amelia = amelia_fit$imputations[[1]]

###########################################
#lasso min selected features + age
###########################################
# combined_bucket_lasso_min = Reduce(function(x, y) merge(x, y, by="bblid", all.x=TRUE), 
#                                         list(Environment_bucket_trimmed[,c("bblid","MedianFamilyIncome","PercentHighSchoolPlus")],
#                                              Demographics_bucket[,c("bblid","sex","race2_White", "age")],
#                                              Family_bucket[,c("bblid","AvgParentEducation","Ran_substance_FH","Parents_Sep_Divorce","Ran_Sui_attempt_or_death_FH")],
#                                              Trauma_bucket[,c("bblid","ptd006")],
#                                              Clinical_bucket[,c("bblid","sui002","man004","add016","add022","sip028","sip038","phb004","add012")],
#                                              Cognitive_bucket[,c("bblid","vmem_s_ar_z","vmem_ar_z","abf_ar_z")]))
# 
# set.seed(42)
# amelia_fit <- amelia(combined_bucket_lasso_min ,m=1,  idvars=c("bblid"), 
#                      ords = c("sex","race2_White",
#                               "Ran_substance_FH","Parents_Sep_Divorce","Ran_Sui_attempt_or_death_FH",
#                               "ptd006",
#                               "sui002","man004","add016","add022","sip028","sip038","phb004","add012"
#                      ))
# 
# summary(amelia_fit)
# 
# combined_bucket_lasso_min_amelia = amelia_fit$imputations[[1]]
# summary(combined_bucket_lasso_min_amelia[,-1])

###########################################
#Lasso and ridge with CV 
###########################################

# x_total = merge(Y_bucket, combined_bucket_lasso_selected_amelia)
# x_total = merge(Y_bucket, combined_bucket_lasso_min_amelia)
# 
# 
# y = x_total[, c(2:5)]
# x = x_total[,-c(1:5)]
# 
# run_lasso(x,y,2)
# run_ridge(x,y)

#######################
#get ridge final model
#######################
# combined_bucket_lasso_selected_amelia_scale = combined_bucket_lasso_selected_amelia
# combined_bucket_lasso_selected_amelia_scale[,c("MedianFamilyIncome","PercentHighSchoolPlus","PWhite","age", "goassessPhqDurMonths", "AvgParentEducation")] = scale(combined_bucket_lasso_selected_amelia_scale[,c("MedianFamilyIncome","PercentHighSchoolPlus","PWhite",
#                                                                                                                                                                                                                   "age","goassessPhqDurMonths",
#                                                                                                                                                                                                                 "AvgParentEducation")])
# x_total = merge(Y_bucket, combined_bucket_lasso_selected_amelia_scale)
# y = x_total[, c(2:5)]
# x = x_total[,-c(1:5)]
# 
# set.seed(42)
# lambdas <- 10^seq(3, -2, by = -.1)
# #find best lambda
# mod <- cv.glmnet(as.matrix(x),y$Lifetime_Suicide_Attempt,alpha=0,family="binomial",lambda=lambdas)
# opt_lambda <- mod$lambda.min
# 
# #get model
# mod <- mod$glmnet.fit
# 
# #get selected features 
# round(coef(mod, s=opt_lambda), digits = 3)
# round(exp(coef(mod, s=opt_lambda)), digits = 3)


#######################
#get glm final model
#######################
#scale data set

# x = merge(Y_bucket, combined_bucket_lasso_selected_amelia_scale)
# mini_bucket = combined_bucket_lasso_selected_amelia_scale[,-1]
# 
# 
# resids = create_resids(mini_bucket)
# # add residual columns to data frame
# x <- data.frame(x,resids)
# 
# set.seed(42)
# mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
# summary(mod_resid)
# 
# data.frame(
#   Estimate    = round(       summary(mod_resid)$coef[ , "Estimate"] , digits = 3),
#   EffectSize  = round(exp(   summary(mod_resid)$coef[ , "Estimate"]), digits = 3),
#   CI_2_5      = round(exp(confint.default(mod_resid)[ ,   1     ]  ), digits = 3),
#   CI_97_5     = round(exp(confint.default(mod_resid)[ ,   2     ]  ), digits = 3),
#   Pvalue      = round(       summary(mod_resid)$coef[ , "Pr(>|z|)"] , digits = 3) 
# )




