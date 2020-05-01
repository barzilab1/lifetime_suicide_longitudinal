#get ranking

# res_rf <- read_csv("res_rf.csv")
# res_Relieff <- read_csv("res_Relieff.csv")
# res_lasso <- read_csv("res_lasso.csv")


res_lasso = data.frame(score_lasso = res_lasso, rank_lasso = rank(res_lasso, ties.method= "max"))
res_lasso$feature = rownames(res_lasso)
res_lasso[res_lasso$score_lasso == 0, "rank_lasso"] = 0
res_Relieff = data.frame(score_Relieff = res_Relieff, rank_Relieff = rank(res_Relieff, ties.method= "max"))
res_Relieff$feature = rownames(res_Relieff)
res_Relieff[res_Relieff$score_Relieff == 0, "rank_Relieff"] = 0
res_rf = data.frame(score_rf = res_rf[,1], rank_rf = rank(res_rf, ties.method= "max"))
res_rf$feature = rownames(res_rf)

all_features = merge(res_lasso,res_Relieff)
all_features = merge(all_features,res_rf)

all_features$mean_rank = round(rowMeans(all_features[,c("rank_lasso", "rank_Relieff", "rank_rf")]), digits = 3)

all_features$scale_lasso = scale(all_features$score_lasso)
all_features$scale_Relieff = scale(all_features$score_Relieff)
all_features$scale_rf = scale(all_features$score_rf)
all_features$sum_scale = scale(rowSums(all_features[,c("scale_lasso","scale_Relieff", "scale_rf")]))


all_features$score_rf_scaled = (all_features$score_rf*10000)/max(all_features$score_rf)
all_features$mean_score = round(rowMeans(all_features[,c("score_lasso", "score_Relieff", "score_rf_scaled")]), digits = 3)


write.csv(all_features, "all.csv")


