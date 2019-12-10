Full_Data= Reduce(function(x, y) merge(x, y, by="bblid"), list(PHQ_Data, 
                                                               PNC_Core_Data_environment, 
                                                               PNC_Core_Data_cognitive, 
                                                               PNC_Core_Data_cognitive_ALTERNATIVE,
                                                               PNC_Core_Data_clinical,
                                                               PNC_Core_Data_demographics))

write.csv(Full_Data,file = "Full_Data.csv", row.names = FALSE)

number_NA = sum(is.na(Full_Data))
number_NA_col = colSums(is.na(Full_Data))

hist(Full_Data$AGE_YEARS_AT_CONTACT)
hist(Full_Data$goassessPhqDurMonths)

#install.packages("reshape2")
#library(reshape2)

library("PerformanceAnalytics")
chart.Correlation(mydata, histogram=TRUE, pch=19)

install.packages("caret")
findCorrelation(x, cutoff = 0.9, verbose = TRUE, names = TRUE, exact = ncol(x) < 15)

summary(Demographics_bucket)
lapply(Demographics_bucket,class)
str(Demographics_bucket)

#correllation matrix
cormat = cor(mydata)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()


# turn a date into a 'monthnumber' relative to an origin
monnb = function(d) { 
  lt = as.POSIXlt(d, format = "%m/%d/%Y") 
  lt$year*12 + lt$mon 
} 
# compute a month difference as a difference between two monnb's
mondf = function(d1, d2) { monnb(d2) - monnb(d1) }

hist(mondf(Full_Data$go1_goassess_date, Full_Data$CONTACT_DATE))

sum(is.na(Family_bucket$Parents_Sep_Divorce))



Full_Cognitive= merge(PHQ_Data, PNC_Core_Data_cognitive_ALTERNATIVE, by="bblid")
number_N =length(which(Full_Cognitive == 'N'))
length(which(Full_Cognitive == 'N', arr.ind = TRUE))


table(Full_Data$Lifetime_Suicide_Attempt)
table(Full_Data$sui002)
table(Full_Data$Current_Suicidal_Ideation)
length(which(Full_Data$sui002 == 1 & Full_Data$Current_Suicidal_Ideation == 2))
length(which(Full_Data$sui002 == 1 & Full_Data$Lifetime_Suicide_Attempt == 2))
length(which(Full_Data$sui002 == 1 & Full_Data$Lifetime_Suicide_Attempt == 2 & Full_Data$Current_Suicidal_Ideation == 2))

