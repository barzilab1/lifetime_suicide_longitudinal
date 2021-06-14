#' step 1: 
#' (1) run algo and save all auc 
x_total = merge(Y_bucket, combined_bucket)

y = x_total[,2]
x = x_total[,-c(1:5)]

auc_results = run_rf_ridge(x,y, buckets_features_names)
ridge_auc = auc_results$ridge_auc
rf_auc = auc_results$rf_auc

#' (2) check significance 
cat("\nridge")
cat("\n\nclinical")
check_significance(ridge_auc$clinical, as.data.frame(ridge_auc)[,c("demographics","family","cognitive","environment","trauma")],"ridge")
cat("\ncombined")
check_significance(ridge_auc$all, as.data.frame(ridge_auc)[,c("demographics","family","cognitive","environment","trauma","clinical")],"ridge")

cat("\nRF")
cat("\n\nclinical")
check_significance(rf_auc$clinical, as.data.frame(rf_auc)[,c("demographics","family","cognitive","environment","trauma")], "RF")
cat("\ncombined")
check_significance(rf_auc$all,  as.data.frame(rf_auc)[,c("demographics","family","cognitive","environment","trauma","clinical")], "RF")
