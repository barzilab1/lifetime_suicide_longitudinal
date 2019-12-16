
summary(truma_bucket)
chart.Correlation(truma_bucket[,-1], histogram=TRUE, pch=19)

table(truma_bucket$ptd005) # total: 14 
table(truma_bucket$ptd004) # total: 15
# one is miising in ptd005
which(truma_bucket$ptd004 == 1 & is.na(truma_bucket$ptd005) ) # [641]
#TODO ask! create new var (4 or 5)?
#first populate with 0. amelia keeps mean so populates with a lot of 1
#fill all NA in ptd005 with 0: if wasn't ask bacouse ptd004 == 0, then it is 0 else [641]? 
truma_bucket$ptd005[is.na(truma_bucket$ptd005)] = 0

# find empty rows exclude 005
empty_row_indexes = which(is.na(truma_bucket$ptd001) & is.na(truma_bucket$ptd002) & is.na(truma_bucket$ptd003) & 
                            + is.na(truma_bucket$ptd004) & is.na(truma_bucket$ptd006) & is.na(truma_bucket$ptd007) &
                            + is.na(truma_bucket$ptd008) & is.na(truma_bucket$ptd009))
#remove empty rows
truma_bucket = truma_bucket[-empty_row_indexes,]

summary(truma_bucket)
chart.Correlation(truma_bucket[,-1], histogram=TRUE, pch=19)

#also removes empty rows 
amelia_fit <- amelia(truma_bucket,idvars=c("bblid"), noms = c("ptd001","ptd002", "ptd003","ptd004","ptd005",
                                                              "ptd006","ptd007","ptd008","ptd009"))

summary(amelia_fit)


