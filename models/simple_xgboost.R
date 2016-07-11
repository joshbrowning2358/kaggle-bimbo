library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("~/Personal Files/Kaggle/ModelFramework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")

cnames = fread("data/train.csv", nrow=0)
# train = fread("data/train.csv", col.names=colnames(cnames),
#               colClasses=c("numeric", rep("factor", 5), rep("numeric", 5)))
# train_wk9 = train[Semana == 9, ]
# train = fread("data/train_sample_7.5M.csv", col.names=colnames(cnames),
#               colClasses=c("numeric", rep("factor", 5), rep("numeric", 5)))
# train = train[Semana != 9, ]
# test = fread("data/test.csv")

train = fread("data/train_complete_45k_features.csv")
test = fread("data/test_1000_features.csv", nrow=1000)


model = "XGB_product_agency"

formula = dummy ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + has_vanilla +
    has_choco + pieces + weight_per_piece + factor(Town) + factor(brand) + 0
rmsleObjective <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  # rmsle = sqrt(1/n * sum((log(pred + 1) - log(actual + 1))^2))
  #
  # rmsle' = 1/(2*sqrt(1/n * sum((log(pred + 1) - log(actual + 1))^2))) * 1/(n * (pred + 1)) = grad
  #
  # rmsle'' = -1/(4*sqrt(1/n * sum((log(pred + 1) - log(actual + 1))^2))^3) * 1/(n * (pred + 1)) +
  #     1/(2*sqrt(1/n * sum((log(pred + 1) - log(actual + 1))^2))) * (-n)/(n * (pred + 1))^2 = hess
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}
fitModel = function(X, y){
    X[, dummy := 1]
    X = sparse.model.matrix(formula, data=X)
    xgboost(data=X, label=y, nrounds=10)
}
predModel = function(model, X){
    X[, dummy := 1]
    X = sparse.model.matrix(formula, data=X)
    xgboost::predict(model, X, params=list(objective)
}
cv = crossValidation(model=list(predict=predModel, fit=fitModel),
                    xTrain = train,
                    yTrain = train[, Demanda_uni_equil],
                    xTest = test,
                    cvIndices = train[, Semana])
summary(cv)
run(cv, metric=rmsle, plotResults=FALSE, logged=FALSE)
