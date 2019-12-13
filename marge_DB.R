#read the CSV. 
#TODO: fix columns classes using colClasses 
PHQ_Data = read_csv("PHQ9_file_for_Elina_with_PNC_FH.csv")
PNC_Core_Data_clinical = read_csv("PNC_Core_Data_clinical.csv")
PNC_Core_Data_cognitive = read_csv("PNC_Core_Data_cognitive.csv")
PNC_Core_Data_cognitive_ALTERNATIVE = read_csv("PNC_Core_Data_cognitive_ALTERNATIVE.csv")
PNC_Core_Data_demographics = read_csv("PNC_Core_Data_demographics.csv")
PNC_Core_Data_environment = read_csv("PNC_Core_Data_environment.csv")
SepDivorce = read_csv("SepDivorce.csv")


#build buckets 
Y_bucket = PHQ_Data[,c("bblid","Current_Suicidal_Ideation","Lifetime_Suicide_Attempt" , "Depression_mod_above_at_phq", "PHQ9_Sum")]

Family_bucket = PHQ_Data[,c("bblid","Ran_psychosis_FH","Ran_depression_FH","Ran_bipolar_or_lithium_FH","Ran_Sui_attempt_or_death_FH","Ran_substance_FH")] 
Family_bucket = merge(Family_bucket, PNC_Core_Data_demographics[,c(1,9:11)])
Family_bucket = merge(Family_bucket, SepDivorce)

Environment_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_environment[,c(1:16)])
Truma_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_environment[,-c(2:16)])  

Demographics_bucket = merge(PHQ_Data[,c("bblid", "goassessPhqDurMonths")], PNC_Core_Data_demographics[,c(1,2,4:8)])

Clinical_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_clinical)
Cognitive_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive)
Cognitive_raw_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive_ALTERNATIVE)


##Y_bucket
Y_bucket[,c("Current_Suicidal_Ideation","Lifetime_Suicide_Attempt")] = Y_bucket[,c("Current_Suicidal_Ideation","Lifetime_Suicide_Attempt")] -1







