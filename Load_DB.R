library("readr")

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
#remove empty rows
Y_bucket = Y_bucket[!is.na(Y_bucket$Current_Suicidal_Ideation),]

Family_bucket = PHQ_Data[!is.na(PHQ_Data$Current_Suicidal_Ideation),
                         c("bblid","Ran_psychosis_FH","Ran_depression_FH","Ran_bipolar_or_lithium_FH","Ran_Sui_attempt_or_death_FH","Ran_substance_FH")] 
Family_bucket = merge(Family_bucket, PNC_Core_Data_demographics[,c(1,9:11)])
Family_bucket = merge(Family_bucket, SepDivorce)

Environment_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_environment[,c(1:16)])

Trauma_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_environment[,-c(2:16)])  

Demographics_bucket = merge(PHQ_Data[,c("bblid", "goassessPhqDurMonths")], PNC_Core_Data_demographics[,c(1,2,4:8)])
Demographics_bucket = merge(Y_bucket[,c("bblid")],Demographics_bucket )

Clinical_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_clinical)

Cognitive_raw_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive_ALTERNATIVE)
Cognitive_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive)


##Y_bucket
Y_bucket[,c("Current_Suicidal_Ideation","Lifetime_Suicide_Attempt")] = Y_bucket[,c("Current_Suicidal_Ideation","Lifetime_Suicide_Attempt")] -1


#check na%
Full_Data= Reduce(function(x, y) merge(x, y, by="bblid"), list(PHQ_Data, 
                                                               PNC_Core_Data_environment, 
                                                               PNC_Core_Data_cognitive, 
                                                               PNC_Core_Data_cognitive_ALTERNATIVE,
                                                               PNC_Core_Data_clinical,
                                                               PNC_Core_Data_demographics,
                                                               SepDivorce))

number_NA = sum(is.na(Full_Data))
number_NA_col = colSums(is.na(Full_Data))
data_dim = dim(Full_Data) 
number_NA/(data_dim[1]*data_dim[2]) #0.11
