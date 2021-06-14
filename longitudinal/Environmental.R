
summary(Environment_bucket[,-1])

#remove neighborhoodCrime. 542 out of 922 (59%) are missing
Environment_bucket = Environment_bucket[,! names(Environment_bucket) %in% c("neighborhoodCrime")]

# MedianFamilyIncome <0 , 556 620 => whill be removed as outliers
describe(Environment_bucket[,-1])
boxplot(Environment_bucket[,-1])

#shift outliers values
Environment_bucket_trimmed = winsor(Environment_bucket[,-1],trim=0.005)
Environment_bucket_trimmed = data.frame(Environment_bucket$bblid, Environment_bucket_trimmed)
colnames(Environment_bucket_trimmed)[1] <- "bblid"

describe(Environment_bucket_trimmed[,-1])
summary(Environment_bucket_trimmed[,-1])
boxplot(Environment_bucket_trimmed[,-1])

Environment_bucket_trimmed