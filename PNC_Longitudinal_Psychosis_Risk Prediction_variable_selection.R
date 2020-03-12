
library(MLmetrics)
library(psych)
library(glmnet)
library(Amelia)
library(caret)
library(caTools)
#library(h2o)
library(glmnetUtils)
library(randomForest)
library(e1071)
library(stir)

############################################################################
# relieff (according to P_value)
############################################################################

nam <- readLines("names9.txt",skip=0)
x <- read.csv("Moore_Psychosis_Risk_Prediction.csv")

x <- x[,nam]

nsim <- 10000

set.seed(2)
temp <- amelia(x[,3:82],m=1,ords=seq(51,80,1))$imputations[[1]]

x[,3:82] <- temp

x <- x[is.na(x$psTerminal)==FALSE,]

results <- NA

for (i in 1:nsim) {

samp <- sample(c(TRUE,FALSE),nrow(x),replace=TRUE)
#samp <- sample.split(x$psTerminal, SplitRatio = .50)
xtr <- subset(x, samp == TRUE)
ytr <- xtr$psTerminal
xtr <- xtr[,2:dim(xtr)[2]]

RF.method = "multisurf"
metric <- "manhattan"
neighbors <- find.neighbors(xtr, ytr, k = 0, method = RF.method)
res <- stir(xtr, neighbors, k = 0, metric = metric, method = RF.method)$STIR_T
res <- rownames(res[res$t.pval < 0.05,])

try(results <- c(results,res))
}

table(results)
write.csv(table(results),"re-split_relieff_best_vars.csv")


############################################################################
# random forest
############################################################################

nam <- readLines("names9.txt",skip=0)
x <- read.csv("Moore_Psychosis_Risk_Prediction.csv")

x <- x[,nam]

nsim <- 10000

set.seed(2)
temp <- amelia(x[,3:82],m=1,ords=seq(51,80,1))$imputations[[1]]

x[,3:82] <- temp

x <- x[is.na(x$psTerminal)==FALSE,]

results <- matrix(NA,ncol(x)-1,nsim)

for (i in 1:nsim) {

samp <- sample(c(TRUE,FALSE),nrow(x),replace=TRUE)
#samp <- sample.split(x$psTerminal, SplitRatio = .50)
xtr <- subset(x, samp == TRUE)
ytr <- xtr$psTerminal
xtr <- xtr[,2:dim(xtr)[2]]

mod1 <- randomForest(x=as.matrix(xtr),y=as.factor(ytr))

res <- as.matrix(mod1$importance)
results[,i] <- scale(res)
}

cbind(colnames(xtr),rowMeans(results))
write.csv(cbind(colnames(xtr),rowMeans(results)),"re-split_randomforest_best_vars.csv")


############################################################################
# bootstrapped lasso
############################################################################

nam <- readLines("names9.txt",skip=0)
x <- read.csv("Moore_Psychosis_Risk_Prediction.csv")

x <- x[,nam]

nsim <- 10000

set.seed(2)
temp <- amelia(x[,3:82],m=1,ords=seq(51,80,1))$imputations[[1]]

x[,3:82] <- temp

x <- x[is.na(x$psTerminal)==FALSE,]

results <- NA

for (i in 1:nsim) {
samp <- sample(c(TRUE,FALSE),nrow(x),replace=TRUE)
#samp <- sample.split(x$psTerminal, SplitRatio = .50)
xtr <- subset(x, samp == TRUE)
ytr <- xtr$psTerminal
xtr <- xtr[,2:dim(xtr)[2]]

lambda <- cv.glmnet(x=as.matrix(xtr),y=as.matrix(ytr),family="binomial",alpha=1)$lambda.min
mod1 <- glmnet(x=as.matrix(xtr),y=as.matrix(ytr),family="binomial",alpha=1,lambda=lambda)
res <- as.matrix(coef(mod1))
try(results <- c(results,rownames(res)[res!=0]))
}

write.csv(table(results),"re-split_lasso_best_vars.csv")

