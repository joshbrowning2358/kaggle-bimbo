library(data.table)
library(glmnet)
options(scipen=500)

sapply(dir("../Model Framework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")

# train = fread("data/train.csv")
# test = fread("data/test.csv")

train = fread("data/train_complete_45k_features.csv")
test = fread("data/test_1000_features.csv", nrow=1000)

predModel = function(model, X){
    meanCols = grep("mean_", colnames(X), value=TRUE)
    trendCols = grep("trend_", colnames(X), value=TRUE)
    sapply(meanCols, function(col) X[is.na(get(col)), c(col) := mean_])
    sapply(trendCols, function(col) X[is.na(get(col)), c(col) := trend_])
    exp(predict(model$glmnet.fit,
                newx=as.matrix(X[, c(meanCols, trendCols), with=FALSE]),
                s=model$lambda.1se)) - 1
}
fitModel = function(X, y){
    X[, y := as.numeric(y)]
    meanCols = grep("mean_", colnames(X), value=TRUE)
    trendCols = grep("trend_", colnames(X), value=TRUE)
    sapply(meanCols, function(col) X[is.na(get(col)), c(col) := mean_])
    sapply(trendCols, function(col) X[is.na(get(col)), c(col) := trend_])
    fit = cv.glmnet(x=as.matrix(X[, c(meanCols, trendCols), with=FALSE]),
                    y=log(y+1), alpha=0)
    return(fit)
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train[Semana!=3, ],
                              yTrain = train[Semana!=3, Demanda_uni_equil],
                              xTest = test,
                              cvIndices = train[Semana!=3, Semana])
summary(productoAvg)
run(productoAvg, filename="hierarchical_log_means", metric=RMSLE, logged=FALSE)

pred <- fread('hierarchical_log_means_full.csv')
pred <- data.table(id = as.character(1:nrow(pred) - 1), Demanda_uni_equil = pred$x)
pred <- data.frame(pred)
write.csv(pred, file='hierarchical_log_means_full.csv', row.names=FALSE)