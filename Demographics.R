#add the phq age
Demographics_bucket$ageAtPHQAssess2 = Demographics_bucket$ageAtClinicalAssess1 + Demographics_bucket$goassessPhqDurMonths
#remove the goassessPhqDurMonths
Demographics_bucket = subset(Demographics_bucket, select=-c(goassessPhqDurMonths))


summary(Demographics_bucket)

#missing cnb age will be the go assess 1 age
Demographics_bucket$ageAtCnb1[is.na(Demographics_bucket$ageAtCnb1)] = Demographics_bucket$ageAtClinicalAssess1[is.na(Demographics_bucket$ageAtCnb1)]

#missing edu1 will be the mean of the edu of kids at the same age 
na_edu_indexes = which(is.na(Demographics_bucket$edu1))

for ( i in na_edu_indexes){
  Demographics_bucket$edu1[i] = round(mean(Demographics_bucket$edu1[
    Demographics_bucket$ageAtClinicalAssess1 == Demographics_bucket$ageAtClinicalAssess1[i]], na.rm = TRUE),2)
}

#correllation matrix
cormat = cor(Demographics_bucket)
melted_cormat = melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

chart.Correlation(Demographics_bucket, histogram=TRUE, pch=19)
pairs.panels(Demographics_bucket, scale=TRUE)