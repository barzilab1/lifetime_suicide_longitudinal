library(h2o)
h2o.init()


h2o_combined = as.h2o(x_total[,c(3,6:198)], "h2o_combined")
y_name = names(x_total[3])

h2o_combined[,y_name] = as.factor(h2o_combined[,y_name])

a = h2o.splitFrame(data = h2o_combined, ratios = 0.75)

#TODO change time!!!
aml <- h2o.automl(y = y_name,
                  training_frame = a[[1]],
                  max_runtime_secs = 60,
                  nfolds = 10,
                  stopping_metric= "AUC",
                  seed = 101)
lb <- aml@leaderboard
print(lb, n = nrow(lb))



# Get model ids for all models in the AutoML Leaderboard
# model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# m = h2o.getModel(model_ids[2])
m1 = aml@leader



leader_model = aml@leader


#Examine the variable importance of the metalearner (combiner) algorithm in the ensemble.  This shows us how much each base learner is contributing to the ensemble. The AutoML Stacked Ensembles use the default metalearner algorithm (GLM with non-negative weights), so the variable importance of the metalearner is actually the standardized coefficient magnitudes of the GLM. 
h2o.varimp(leader_model)

#We can also plot the base learner contributions to the ensemble.
h2o.varimp_plot(leader_model,num_of_features = 50)

pred = predict(leader_model,h2o_combined[-c(1:600),])
pred

perf <- h2o.performance(leader_model, a[[2]])
h2o.auc(perf)

