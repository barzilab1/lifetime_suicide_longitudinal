
summary(Clinical_bucket)


#TODO What to do with these 3 rows?  
# 3 rows have only the summary variables [medical vars that might be assessed by a different doc]  
nrow(Clinical_bucket[rowSums(is.na(Clinical_bucket)) > 9,])
# 4 rows missing all the summary variables [gaf- score between 0-100 (good), 50 bad, relates to the goassess]
nrow(Clinical_bucket[rowSums(is.na(Clinical_bucket)) == 8,])

#remove rows with too many NA
x = Clinical_bucket[rowSums(is.na(Clinical_bucket)) < 8,]

# amelia_fit <- amelia(Clinical_bucket,idvars=c("bblid"), noms = c("ptd001","ptd002", "ptd003","ptd004","ptd005",
                                                              # "ptd006","ptd007","ptd008","ptd009"))

# chart.Correlation(Clinical_bucket[,-1], histogram=TRUE, pch=19)
