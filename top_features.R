

demographics_features = c("bblid","sex","age","race2_White","race2_Black","goassessPhqDurMonths")

features_list = list()

lasso_selected_features = all_features[order(all_features$rank_lasso, decreasing = TRUE),c("feature","rank_lasso")]
features_list$lasso = lasso_selected_features$feature[1:res_lasso$number_selected]

relieff_selected_features = all_features[order(all_features$rank_Relieff, decreasing = TRUE),c("feature","rank_Relieff")]
features_list$relieff = relieff_selected_features$feature[1:res_Relieff$number_selected]

rf_selected_features = all_features[order(all_features$rank_rf, decreasing = TRUE),c("feature","rank_rf")]
features_list$rf = rf_selected_features$feature[1:45]

for (j in c("mean_rank")){ #,"mean_score","sum_scale"
  for (i in c(5,10,13,14,15,20,25,30,35)){
  
    col_name = paste(j,i,sep = "_" )
    
    features = all_features[order(all_features[[j]], decreasing = TRUE),c("feature"),drop = F]
    features_list[[col_name]] = features$feature[1:i]
    
  }
  
}

for (i in names(features_list)){
 
  features = union(features_list[[i]] , demographics_features)
  combined = combined_bucket_amelia[,names(combined_bucket_amelia) %in% features]

  cat("\n###########################################\n")
  cat(i)
  cat("\n###########################################\n")

  x_total = merge(Y_bucket, combined)

  y = x_total[, c(2:5)]
  x = x_total[,-c(1:5)]

  run_ridge(x,y[,2])
  run_tree_RF(x,y[,2])

}


# add demographics to the above selected subsets
for (i in names(features_list)){
  features_list[[i]] = union(features_list[[i]] , demographics_features)
}
features_list$demographics = demographics_names
features_list$environment = environment_names
features_list$family = family_names
features_list$trauma = trauma_names
features_list$cognitive = cognitive_names
features_list$clinical = clinical_names
features_list$combined = all_features$feature



# ridge_auc = rf_auc = matrix(NA, nrow = splits ,ncol = length(features_list))
# colnames(ridge_auc) = colnames(rf_auc) = names(features_list)
# 
# x_total = merge(Y_bucket, combined_bucket_amelia)
# 
# y = x_total[,3]
# x = x_total[,-c(1:5)]
# 
# cl = makeCluster(N_CORES, type="FORK")
# registerDoParallel(cl)
# 
# set.seed(42)
# #split the data splits times to 75% training and 25% test
# for (i in 1:splits) {
#   
#   splitz = sample.split(y, .75)
#   
#   #go over each feature list and calculate the auc
#   for (j in names(features_list)){
#     
#     #get data
#     selected_data = x[,names(x) %in% features_list[[j]] ] 
#     
#     x_train <- selected_data[splitz,]
#     y_train <- y[splitz]
#     x_test <- selected_data[!splitz,]
#     y_test <- y[!splitz]
#     
#     ###ridge
#     mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=0,family="binomial", parallel = TRUE)
#     opt_lambda <- mod$lambda.min
#     mod <- mod$glmnet.fit
#     y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
#     pred <- prediction(y_predicted, y_test)
#     
#     ridge_auc[i,j] <- performance(pred, measure = "auc")@y.values[[1]]
#   
#     ###rf
#     mode_rf <- randomForest(x=x_train,y=as.factor(y_train))
#     y_predicted <- predict(mode_rf, newdata = x_test, type ="prob")[,2]
#     pred <- prediction(y_predicted, y_test)
#     
#     rf_auc[i,j] =  performance(pred, measure = "auc")@y.values[[1]]
#   
#   }
#   
# }
# 
# 
# stopCluster(cl)
# 
# #calculate and print results
# cat("\n combined results \n")
# 
# cat("\nauc ridge: \n")
# print(apply(ridge_auc, 2, mean, na.rm = T))
# cat("\nSD: \n")
# print(apply(ridge_auc, 2, SD, na.rm = T))
# cat("\n")
# 
# cat("\nauc RF: \n")
# print(apply(rf_auc, 2, mean, na.rm = T))
# cat("\nSD: \n")
# print(apply(rf_auc, 2, SD, na.rm = T))
# cat("\n")

temp = sapply(features_list, '[', seq(max(lengths(features_list))))
write.csv(temp, "features_list.csv", na = "")
# write.csv(ridge_auc, "ridge_auc.csv")
# write.csv(rf_auc, "rf_auc.csv")


#check that clinical is statisticlly higher than rest
#check that combine is higer
#check that lasso and top 25 are higher then combine 

# ridge_auc = read_csv("ridge_auc.csv")[,-1]
# rf_auc = read_csv("rf_auc.csv")[,-1]

# cat("\nridge")
# cat("\n\nclinical")
# check_significance(ridge_auc$clinical, ridge_auc[,c("demographics","family","cognitive","environment","trauma")])
# cat("\ncombined")
# check_significance(ridge_auc$combined, ridge_auc[,c("demographics","family","cognitive","environment","trauma","clinical")])
# cat("\ncmean_rank_25")
# check_significance(ridge_auc$mean_rank_25, ridge_auc[,c("demographics","family","cognitive","environment","trauma","clinical","combined")])
# cat("\nlasso")
# check_significance(ridge_auc$lasso, ridge_auc[,c("demographics","family","cognitive","environment","trauma","clinical","combined","mean_rank_25")])
# 
# cat("\nridge")
# cat("\n\nclinical")
# check_significance(rf_auc$clinical, rf_auc[,c("demographics","family","cognitive","environment","trauma")])
# cat("\ncombined")
# check_significance(rf_auc$combined, rf_auc[,c("demographics","family","cognitive","environment","trauma","clinical")])
# cat("\ncmean_rank_25")
# check_significance(rf_auc$mean_rank_25, rf_auc[,c("demographics","family","cognitive","environment","trauma","clinical","combined")])
# cat("\nlasso")
# check_significance(rf_auc$lasso, rf_auc[,c("demographics","family","cognitive","environment","trauma","clinical","combined","mean_rank_25")])



# features_list = read_csv("features_list.csv")
df = matrix(nrow = 135, ncol = 5, dimnames = list(c(),c("model","feature","bucket","rank","original_rank")))
df[1:13,"model"] = "Lasso\n(n=13)"
df[1:13, c("feature")] = lasso_selected_features$feature[1:13]
df[1:13, c("rank")] = 193 - lasso_selected_features$rank_lasso[1:13]
df[1:13, c("original_rank")] = lasso_selected_features$rank_lasso[1:13]

df[14:65,"model"] = "Relieff\n(n=52)"
df[14:65, c("feature")] = relieff_selected_features$feature[1:52]
df[14:65, c("rank")] = 193 - relieff_selected_features$rank_Relieff[1:52]
df[14:65, c("original_rank")] = relieff_selected_features$rank_Relieff[1:52]

df[66:110,"model"] = "Random Forest\n(n=45)"
df[66:110, c("feature")] = rf_selected_features$feature[1:45]
df[66:110, c("rank")] = 193 - rf_selected_features$rank_rf[1:45]
df[66:110, c("original_rank")] = rf_selected_features$rank_rf[1:45]

df[111:135,"model"] = "Mean Rank\n(n=25)"
df[111:135, c("feature")] = all_features$feature[order(all_features[["mean_rank"]], decreasing = TRUE),drop=F][1:25]
df[111:135, c("rank")] = seq(0,24)
df[111:135, c("original_rank")] = all_features$mean_rank[order(all_features[["mean_rank"]], decreasing = TRUE),drop = F][1:25]


for (i in 1:135) {
  for (j in c("demographics", "environment","family","trauma","cognitive","clinical")){
    if (df[i,"feature"] %in% features_list[[j]]){
      df[i,"bucket"] = j
      next
    }
  }
}



write.csv(df,"graph.csv",row.names = F)


df = as.data.frame(df)
levels(df$bucket)[levels(df$bucket)=="demographics"] = "Demographics"
levels(df$bucket)[levels(df$bucket)=="environment"] = "Neighborhood"
levels(df$bucket)[levels(df$bucket)=="family"] = "Family"
levels(df$bucket)[levels(df$bucket)=="cognitive"] = "Neurocognitive"
levels(df$bucket)[levels(df$bucket)=="clinical"] = "Clinical"

df$model <- factor(df$model, levels = c("Lasso\n(n=13)","Random Forest\n(n=45)","Relieff\n(n=52)","Mean Rank\n(n=25)"))


ggplot(data = df , mapping = aes(x = model, fill = bucket)) +  
  geom_bar( width = 0.7, position="fill") +
  labs( y = "Percent of features in subset" , fill = "", x ="") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=10, colour = "black"), 
        legend.text=element_text(size=10)) +
  scale_fill_manual(values=c("#F5B7B1","#ABEBC6","#AED6F1","#D2B4DE","#FAD7A0"))


