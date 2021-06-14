
#remove empty rows from origin cog
summary(Cognitive_bucket)
Cognitive_bucket = Cognitive_bucket[rowSums(is.na(Cognitive_bucket)) < 26,]

#remove bblid with battery_valid_collapsed == N
Cognitive_bucket = merge(Cognitive_bucket,Cognitive_raw_bucket[,c("bblid","battery_valid_collapsed")])
table(Cognitive_bucket$battery_valid_collapsed) #N=9
Cognitive_bucket = Cognitive_bucket[Cognitive_bucket$battery_valid_collapsed != "N",]
Cognitive_bucket = Cognitive_bucket[,! names(Cognitive_bucket) %in% c("battery_valid_collapsed")]


Cognitive_bucket