install.packages("readr")
install.packages("regclass")s
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

#correllation matrix
library("PerformanceAnalytics")
chart.Correlation(mydata, histogram=TRUE, pch=19)

cormat = cor(Demographics_bucket)
melted_cormat = melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
#pairs.panels(Demographics_bucket, scale=TRUE)

install.packages("caret")
findCorrelation(x, cutoff = 0.9, verbose = TRUE, names = TRUE, exact = ncol(x) < 15)


#get index of a column 
which(names(Cognitive_bucket_combined)=="er40_m_rtcr")
x = x[!is.na(x$Current_Suicidal_Ideation),]


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
# or
table(x[,27:28])


#get sum of all columns by binary feature 
# dt[subset rows, fun to calculate, grouped by]
dt[, lapply(.SD, sum, na.rm=TRUE), by = above11]


#################### confusionMatrix
library(e1071)
library(ROCR)

y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")

y_predicted_class = ifelse(y_predicted>=cutoff,1,0)

sensitivity_res[i,j] = confusionMatrix(as.factor(y_predicted_class),reference = as.factor(y_test), positive = "1")$byClass["Sensitivity"]

results[i,j] = mean(y_predicted==y_test)


#################### cutoff
library(e1071)
library(caret)
library(ROCR)

y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")

pred <- prediction(y_predicted, y_test)
# calculate cutoff with max sensitivity 
perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) 
plot(x=perf@x.values[[1]],y=perf@y.values[[1]],colorize=TRUE)
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

### plot sensitivity vs specificity
plot(x = perf@alpha.values[[1]], y=(perf@y.values[[1]]), col="red", type="l", xlab= perf@alpha.name, ylab= perf@y.name ,lwd=2 ) 
par(new=TRUE)
plot(x = perf@alpha.values[[1]], y=(1-perf@x.values[[1]]), col="blue", type="l", lwd = 2, xlab="", ylab="") 
axis(4, at=seq(0,1,0.2))
mtext("Specificity",side=4, padj=-2, col='blue')
grid(nx = 15, ny = 10, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

# plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values), 
#      type="l", lwd=2, ylab="Specificity", xlab="Cutoff")
# par(new=TRUE)
# plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values), 
#     type="l", lwd=2, col='red', ylab="", xlab="")
# axis(4, at=seq(0,1,0.2))
# mtext("Specificity",side=4, padj=-2, col='blue')
# grid(nx = 15, ny = 15, col = "lightgray", lty = "dotted",
#      lwd = par("lwd"), equilogs = TRUE)

index.tpr = which(perf@y.values[[1]] == max(perf@y.values[[1]]))
min.fpr = min(perf@x.values[[1]][index.tpr]) 
index.fpr = which(perf@x.values[[1]] == min.fpr)[1] #incase there is more then one index
cutoff[i,j] = perf@alpha.values[[1]][index.fpr]

##### PR CUV
#########################################
library(PRROC)
library(caret)
roc = roc.curve(scores.class0=y_predicted[y_test == 1],scores.class1=y_predicted[y_test == 0], curve = T)  
pr = pr.curve(scores.class0=y_predicted[y_test == 1],scores.class1=y_predicted[y_test == 0], curve = T,  rand.compute = T)
results = opt.cut.pr(pr$curve)
new_measurements["pr",i] = pr$auc.integral
new_measurements["sen",i] = pr$curve[results["ind"],1]
new_measurements["ppv",i] = pr$curve[results["ind"],2]

# ind = which(roc$curve[,3] == results["cutoff"])
new_measurements["auc",i] = roc$auc
# new_measurements["spe",i] = 1 - roc$curve[ind,1]

y_results = ifelse(y_predicted >= results["cutoff"],1,0)
cm = confusionMatrix(data = as.factor(y_results) ,reference = as.factor(y_test), positive = "1")
new_measurements["spe",i] = cm$byClass["Specificity"]
new_measurements["npv",i] = cm$byClass["Neg Pred Value"]


y_ = sum(y_test)/(length(y_test)-sum(y_test))
plot(pr)
lines(c(0,1),c(y_,y_),col = "gray", lty = 4) #baseline




#########
string_name[grepl(paste(string_test, collapse = '|'), string_name)]


##########check if model significant 
pchisq(Residual Deviance, dgree of freedom, lower.tail = FALSE)


################## laso 
set.seed(42)  # Set seed for reproducibility

n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15  # Number of true predictors

## Generate the data
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

## Split data into training and testing datasets.
## 2/3rds of the data will be used for Training and 1/3 of the
## data will be used for Testing.
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

list.of.fits <- list()



## The following loop uses 10-fold Cross Validation to determine the
## optimal value for lambda for alpha = 0, 0.1, ... , 0.9, 1.0
## using the Training dataset.
for (i in 0:10) {
  
  fit.name <- paste0("alpha", i/10)
  
  list.of.fits[[fit.name]] <-
    cv.glmnet(x.train, y.train, type.measure ="mse", alpha=i/10, 
              family="gaussian")
}

## Now we see which alpha (0, 0.1, ... , 0.9, 1) does the best job
## predicting the values in the Testing dataset.
results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  ## Use each model to predict 'y' given the Testing dataset
  predicted <- 
    predict(list.of.fits[[fit.name]], 
            s=list.of.fits[[fit.name]]$lambda.1se, newx=x.test)
  
  ## Calculate the Mean Squared Error...
  mse <- mean((y.test - predicted)^2)
  
  ## Store the results
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}

## View the results
results


#ridge
#take the best lanbda for the model
apply(results, 2, max)
inds = apply(results, 2, which.max)

#Current_Suicidal_Ideation
loc = inds[1]

coefs <- coef(models[[loc]], s=models[[loc]]$lambda)

data.frame(
  features = coefs@Dimnames[[1]][ which(coefs != 0 ) ], #intercept included
  coefs    = coefs              [ which(coefs != 0 ) ]  #intercept included
)

install.packages("h2o")




####prints
cat(sprintf("%s \t %i \n", rownames(features), features$Lifetime_Suicide_Attempt))

for(i in 1:3){
  cat(rownames(features)[i], features$Lifetime_Suicide_Attempt[i], sep="\t")
  cat("\n")
}





#all features selecgted by lasso 
combined_bucket_lasso_selected = Reduce(function(x, y) merge(x, y, by="bblid", all.x=TRUE), 
                                        list(Environment_bucket_trimmed[,c(1:6,9:12,14:15)],
                                             Demographics_bucket[,c(1:3,6:8)],
                                             Family_bucket[,-2],
                                             Trauma_bucket[,c(1,4,5,9)],
                                             Clinical_bucket[,c("bblid","sui002","man004","add016","add022",
                                                                "sip028","sip038","phb004","SIP030",
                                                                "sip011","psy001","add012","scr007",
                                                                "add021","sip027","sip033","man003",
                                                                "man005","sip014","gad002","cdd004",
                                                                "man006","ocd002","sip006","cdd008",
                                                                "ocd001","phb006","SIP035","sip007",
                                                                "sip008","psy071","add011","sui001",
                                                                "man001","phb002","smry_psych_bedwet_rtg",
                                                                "psy050","cdd005","cdd003","cdd007",
                                                                "man007","scr001","agr008","agr003",
                                                                "smry_psych_learning_prob_rtg",
                                                                "smry_psych_speech_prob_rtg",
                                                                "dem107","subs_smry_sub_otc",
                                                                "SIP043","sip010","odd002","odd003",
                                                                "add020","odd001","cdd001","dep002",
                                                                "dep006","ocd005","ocd006","ocd004",
                                                                "ocd019","cdd002","sip039","ocd018",
                                                                "agr002","agr001","soc002","phb005",
                                                                "ocd014","smry_psych_medication_rtg",
                                                                "subs_smry_sub_inh","subs_smry_sub_ster")],
                                             Cognitive_bucket[,c(1:10,13,15,17:21,24:26)]))

set.seed(42)
amelia_fit <- amelia(combined_bucket_lasso_selected ,m=1,  idvars=c("bblid"), 
                     ords = c("sex","race2_White",
                              "Ran_substance_FH","Parents_Sep_Divorce","Ran_Sui_attempt_or_death_FH",
                              "ptd006","ptd0045",
                              "sui002","man004","add016","add022","sip028","sip038","phb004","SIP030","sip011","psy001","add012"
                     ))

summary(amelia_fit)

combined_bucket_lasso_selected_amelia = amelia_fit$imputations[[1]]
summary(combined_bucket_lasso_selected_amelia[,-1])


