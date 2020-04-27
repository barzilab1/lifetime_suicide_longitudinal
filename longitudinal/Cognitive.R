

Cognitive_bucket_combined = merge(Cognitive_bucket, Cognitive_raw_bucket[,c(1,42,46:59,85,90:101)])
setdiff(Y_bucket$bblid, Cognitive_raw_bucket$bblid) #134866 127485 are missing. together with 136049 they dont have any cognetive data

sum(Cognitive_bucket_combined$er40_validcode_collapsed == "N") #7
sum(Cognitive_bucket_combined$er40_validcode_collapsed == "S") #3 (do not have data)
sum(Cognitive_bucket_combined$medf_validcode_collapsed == "N") #12
sum(Cognitive_bucket_combined$medf_validcode_collapsed == "S") #19 (do not have data)
table(Cognitive_bucket_combined$medf_validcode_collapsed, Cognitive_bucket_combined$er40_validcode_collapsed) 

#change tests with validcode = N to NA, total 14 rows. 
Cognitive_bucket_combined[Cognitive_bucket_combined$er40_validcode_collapsed == "N",29:42] = NA 
Cognitive_bucket_combined[Cognitive_bucket_combined$medf_validcode_collapsed == "N",44:55] = NA 

#remove validcode columns 
Cognitive_bucket_combined = Cognitive_bucket_combined[,! names(Cognitive_bucket_combined) %in% 
                                                        c("er40_validcode_collapsed","medf_validcode_collapsed")]

# % of NA. missing 2 rows that appear in the other buckets
number_NA = sum(is.na(Cognitive_bucket_combined[,-1])) #687
data_dim = dim(Cognitive_bucket_combined[,-1]) 
number_NA/(data_dim[1]*data_dim[2]) #0.14

#remove empty rows
Cognitive_bucket_combined = Cognitive_bucket_combined[rowSums(is.na(Cognitive_bucket_combined)) < 52,]

summary(Cognitive_bucket_combined[,-1])
# chart.Correlation(Cognitive_bucket_combined[,-1])
boxplot(Cognitive_bucket_combined[,-1])

# take the log for response time because they are skewed
Z = sapply(Cognitive_bucket_combined[,c(33:37,39,41,43,44,46,47,49,50,52,53)], log)
colnames(Z) <- paste(colnames(Z),"_log",sep="")
Cognitive_bucket_combined = data.frame(Cognitive_bucket_combined,Z)
Cognitive_bucket_combined = Cognitive_bucket_combined[,-c(33:37,39,41,43,44,46,47,49,50,52,53)]

boxplot(Cognitive_bucket_combined[,-1])

# write.csv(cor(Cognitive_bucket_combined[,-1], use = "pairwise"),file = "cor_cog.csv")
#er40_f_rtcr_log & eid_s_ar_z < -.81
#er40_m_rtcr_log & eid_s_ar_z < -.80
#er40_m_cr       & eid_ar_z   > .83
#er40_f_cr       & eid_ar_z   > .79
#medf_hap_cr     & edi_ar_z   > .74
#medf_ang_cr     & edi_ar_z   > .72
#medf_fear_cr    & edi_ar_z   > .72
#medf_hap_rtcr_log  & edi_s_ar_z < -.75
#medf_ang_rtcr_log  & edi_s_ar_z < -.79
#medf_fear_rtcr_log & edi_s_ar_z < -.74
#medf_sad_rtcr_log  & edi_s_ar_z < -.7


# there is perfectly collinearity 28:34 er40_*_cr. remove one of them 
# there is overlap with the data between the sum variables and the raw. remove the sum
Cognitive_bucket_combined = Cognitive_bucket_combined[,! names(Cognitive_bucket_combined) %in% 
                                                        c("er40_m_cr", 
                                                          "eid_s_ar_z","eid_ar_z",
                                                          "edi_s_ar_z","edi_ar_z")]


set.seed(42)
#all variables are numbers. 
amelia_fit <- amelia(Cognitive_bucket_combined ,m=1,  idvars=c("bblid"), ords = (24:33))

summary(amelia_fit)

Cognitive_bucket_amelia = amelia_fit$imputations[[1]]
summary(Cognitive_bucket_amelia[,-1])

#scale only the raw data
Cognitive_bucket_amelia_scaled = Cognitive_bucket_amelia
Cognitive_bucket_amelia_scaled[,c(24:48)] = scale(Cognitive_bucket_amelia_scaled[,c(24:48)])

Cognitive_bucket_combined_scaled = Cognitive_bucket_combined
Cognitive_bucket_combined_scaled[,c(24:48)] = scale(Cognitive_bucket_combined_scaled[,c(24:48)])


########cog without raw
#remove empty rows from origin cog
summary(Cognitive_bucket)
Cognitive_bucket = Cognitive_bucket[rowSums(is.na(Cognitive_bucket)) < 26,]

#remove bblid with battery_valid_collapsed == N
Cognitive_bucket=merge(Cognitive_bucket,Cognitive_raw_bucket[,c("bblid","battery_valid_collapsed")])
table(Cognitive_bucket$battery_valid_collapsed) #N=9
Cognitive_bucket = Cognitive_bucket[Cognitive_bucket$battery_valid_collapsed != "N",]
Cognitive_bucket = Cognitive_bucket[,! names(Cognitive_bucket) %in% c("battery_valid_collapsed")]


#######################################
#Logistic regression 
#######################################

# #amelia data set
# x = merge(Y_bucket,Cognitive_bucket_amelia_scaled)
# cogni_b = Cognitive_bucket_amelia_scaled[,-1]
# 
# #original data set
# x = merge(Y_bucket,Cognitive_bucket_combined_scaled)
# cogni_b = Cognitive_bucket_combined_scaled[,-1]
# 
# 
# resids = create_resids(cogni_b)
# 
# # add residual columns to data frame
# x <- data.frame(x,resids)
# 
# 
# ### Lifetime_Suicide_Attempt
# 
# mod_resid <- glm(Lifetime_Suicide_Attempt~resids,data=x,family="binomial")
# summary(mod_resid)
# get_logistic_results(mod_resid)[-1,]
# pR2(mod_resid)


cat("\n\n###########################################")
print("Cognitive")
###########################################
#Lasso and ridge with CV
###########################################

#amelia data set
x_total = merge(Y_bucket,Cognitive_bucket_amelia)

#original data set
# x_total = merge(Y_bucket,Cognitive_bucket_combined)
# #remove empty rows 
# x_total = x_total[!(rowSums(is.na(x_total)) >= 1),]

#data without raw
x_total = merge(Y_bucket,Cognitive_bucket)


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


