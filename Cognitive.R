
summary(Cognitive_bucket)
nrow(Cognitive_bucket[rowSums(is.na(Cognitive_bucket)) == 26,])

# remove the empty rows
Cognitive_bucket = Cognitive_bucket[rowSums(is.na(Cognitive_bucket)) != 26,]

chart.Correlation(Cognitive_bucket[,-1], histogram=TRUE, pch=19)
