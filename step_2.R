#################################
#' step 2 
#################################

# (1) run the 3 algo on combine bucket
res_lasso = run_lasso(x,y)
res_Relieff = run_stir(x,y)
res_rf = run_RF(x,y)



# (2) get top features from 3 algos and create mean-rank list
ranked_features = rank_features(res_lasso,res_Relieff,res_rf)

features_list = list()

features_list$lasso   =  ranked_features$feature[ranked_features$rank_lasso   > (193-res_lasso$number_selected)]
features_list$relieff =  ranked_features$feature[ranked_features$rank_Relieff > (193-res_Relieff$number_selected)]

print(paste0("\nNumber chosen lasso: ", length(features_list$lasso)))
print(paste0("\nNumber chosen relieff: ", length(features_list$relieff)))


rf_index = find_biggest_gap(res_rf$features[,1])
features_list$rf = rownames(res_rf$features)[1:rf_index]

# (3) get subset from mean rank
mean_rank_features = ranked_features[order(ranked_features[["mean_rank"]], decreasing = TRUE),c("feature"),drop = F]
for (i in c(5,10,15,20,25,30,35)){
  
  col_name = paste0("mean_rank_",i )
  features_list[[col_name]] = mean_rank_features$feature[1:i]
  
}
  




lasso_index = length(features_list$lasso)
relieff_index = length(features_list$relieff)

ind = 1
#print 
df = matrix(nrow = (lasso_index+relieff_index+rf_index+25), ncol = 5, dimnames = list(c(),c("model","feature","bucket","rank","original_rank")))
df[ind:lasso_index,"model"] = paste0("Lasso\n(n=",lasso_index,")")
df[ind:lasso_index, c("feature")] = features_list$lasso

ind = lasso_index
df[(ind +1):(ind+relieff_index),"model"] = paste0("Relieff\n(n=",relieff_index,")")
df[(ind +1):(ind+relieff_index), c("feature")] = features_list$relieff

ind = ind + relieff_index
df[(ind +1):(ind+rf_index),"model"] = paste0("Random Forest\n(n=",rf_index,")")
df[(ind +1):(ind+rf_index), c("feature")] = features_list$rf

ind = ind + rf_index
df[(ind +1):(ind+25),"model"] = "Mean Rank\n(n=25)"
df[(ind +1):(ind+25), c("feature")] = features_list$mean_rank_25


for (i in 1:nrow(df)) {
  for (j in c("demographics", "environment","family","trauma","cognitive","clinical")){
    if (df[i,"feature"] %in% buckets_features_names[[j]]){
      df[i,"bucket"] = j
      next
    }
  }
}



write.csv(df,"graph_data.csv",row.names = F)


df = as.data.frame(df)
levels(df$bucket)[levels(df$bucket)=="demographics"] = "Demographics"
levels(df$bucket)[levels(df$bucket)=="environment"] = "Neighborhood"
levels(df$bucket)[levels(df$bucket)=="family"] = "Family"
levels(df$bucket)[levels(df$bucket)=="cognitive"] = "Neurocognitive"
levels(df$bucket)[levels(df$bucket)=="clinical"] = "Clinical"

levels_df = unique(df$model)

df$model <- factor(df$model, levels = levels_df)


ggplot(data = df , mapping = aes(x = model, fill = bucket)) +  
  geom_bar( width = 0.7, position="fill") +
  labs( y = "Percent of features in subset" , fill = "", x ="") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#F5B7B1","#ABEBC6","#AED6F1","#D2B4DE","#FAD7A0", "#F1BDAE"))+
  theme(axis.text=element_text(size=10, colour = "black"), 
        legend.text=element_text(size=10)) 
  


