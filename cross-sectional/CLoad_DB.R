
#read the CSV. 
PNC_over_11 = read_csv("Data/PNC over 11 file for Elina with PNC FH.csv")

##################################
#### buckets 
##################################

###Y_bucket
Y_bucket_cross = PNC_Core_Data_clinical[,c("bblid", "sui002")]
Y_bucket_cross = merge(PNC_over_11[,c("bblid")], Y_bucket_cross)
#remove rows that are part of the longitudinal
Y_bucket_cross = Y_bucket_cross[ !(Y_bucket_cross$bblid %in% Y_bucket$bblid),]
#remove rows with no data
Y_bucket_cross = Y_bucket_cross[ !is.na(Y_bucket_cross$sui002),]
summary(Y_bucket_cross)
table(Y_bucket_cross$sui002) #n=660, 1%


###EDU
Edu_info_cross = merge(GOASSESS_Timeline[,1:2],GO1_grade_repeats)
#get bblid
t = do.call('rbind', strsplit(as.character(Edu_info_cross$redcapid),'_',fixed=TRUE))
Edu_info_cross$bblid = t[,1]
Edu_info_cross$suffix = t[,2]
Edu_info_cross = merge(Y_bucket_cross[,c("bblid"), drop=FALSE],Edu_info_cross)

#combine rows with the same bblid
Edu_info_combined_cross = aggregate(Edu_info_cross[,c("tml007","dem107","dem108","dem109")], list(bblid=Edu_info_cross$bblid), function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
})

#find contradictions between parent and child answer
double_value_indexes = which(lengths(Edu_info_combined_cross$tml007) > 1)
#take the parent answer
for ( i in double_value_indexes){
  Edu_info_combined_cross$tml007[i] = Edu_info_cross$tml007[Edu_info_cross$bblid == Edu_info_combined_cross$bblid[i] & Edu_info_cross$suffix == "MI"]
}
Edu_info_combined_cross$tml007 = unlist(Edu_info_combined_cross$tml007)


###Family_bucket
Family_bucket_cross = merge(Y_bucket_cross[,c("bblid"), drop=FALSE], PNC_over_11)
Family_bucket_cross = merge(Family_bucket_cross, PNC_Core_Data_demographics[,c(1,9:11)])
Family_bucket_cross = merge(Family_bucket_cross, SepDivorce)

###Environment_bucket
Environment_bucket_cross = merge(Y_bucket_cross[,c("bblid"), drop=FALSE], PNC_Core_Data_environment[,c(1:16)])

###Trauma_bucket
Trauma_bucket_cross = merge(Y_bucket_cross[,c("bblid"), drop=FALSE], PNC_Core_Data_environment[,-c(2:16)])  

###Demographics_bucket
Demographics_bucket_cross = merge(Y_bucket_cross[,c("bblid"), drop=FALSE], PNC_Core_Data_demographics[,c(1,2,4:8)])
Demographics_bucket_cross = merge(Demographics_bucket_cross, Edu_info_combined_cross[,c("bblid","tml007")])

###Clinical_bucket
Clinical_bucket_cross = merge(Y_bucket_cross[,c("bblid"), drop = FALSE], PNC_Core_Data_clinical)
#remove ptd009.x and sui002
Clinical_bucket_cross = Clinical_bucket_cross[,! names(Clinical_bucket_cross) %in% c("ptd009.x", "sui002")]
Clinical_bucket_cross = merge(Sips_Extras, Clinical_bucket_cross)
#add if the child skipped a class 
Clinical_bucket_cross = merge(Clinical_bucket_cross, Edu_info_combined_cross[,c(1,3)])
Substance_bucket_cross = merge(Y_bucket_cross[,c("bblid"), drop = FALSE], GO1_Substance_Use[,c("bblid","subs_smry_sub_stim","subs_smry_sub_inh","subs_smry_sub_otc","subs_smry_sub_ster","AlcUse")])

Cognitive_bucket_cross = merge(Y_bucket_cross[,c("bblid"), drop = FALSE], PNC_Core_Data_cognitive)

