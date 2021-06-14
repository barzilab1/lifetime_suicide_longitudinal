
summary(Clinical_bucket[,-1])

#remove ptd009.x=trauma
Clinical_bucket = Clinical_bucket[,-which(colnames(Clinical_bucket) == "ptd009.x")]

# 3 rows have only the summary variables [medical vars that might be assessed by a different doc]  
t = Clinical_bucket[,c(2:117)]
nrow(t[rowSums(is.na(t)) == 116,])
# 4 rows missing all the summary variables [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
t = Clinical_bucket[,c(119:126)]
nrow(t[rowSums(is.na(t)) == 8,])


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
# lm(data=t[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc)),], remaining_sum ~ subs_smry_sub_mar )
# which(t$subs_smry_sub_mar[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc))] != t$remaining_sum[(t$subs_smry_sub_alc==0 | is.na(t$subs_smry_sub_alc))])

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
Clinical_bucket_full = merge(x=Clinical_bucket,y=Substance_bucket,all.x = TRUE)

summary(Clinical_bucket_full)

#get cor including Y
# write.csv(cor_auto(merge(Clinical_bucket,Y_bucket)) ,file = "cor_clinic.csv")

Clinical_bucket_full

