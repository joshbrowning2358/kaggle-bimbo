library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("../ModelFramework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("rmsle.R")

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


formula = "target ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + has_vanilla +
    has_choco + factor(Town) + factor(brand) + 0"
fitModel = function(X, y){
    X[, target := y]
    X = sparse.model.matrix(as.formula(formula), data=X)
    # Objective of MSE with log(y) <=> RMSE with log(y) <=> RMLSE with y
    xgboost(data=X, label=log(y + 1), nrounds=20)
}
predModel = function(model, X){
    X[, index := 1:.N]
    X[, target := -9999]
    formula = paste0(formula, "+ index")
    X = sparse.model.matrix(as.formula(formula), data=X)
    preds = xgboost::predict(model, X)
    (exp(preds) - 1)[X[, colnames(X) == "index"]]
}
cv = crossValidation(model=list(predict=predModel, fit=fitModel),
                    xTrain = train,
                    yTrain = train[, Demanda_uni_equil],
                    xTest = test,
                    cvIndices = train[, Semana])
summary(cv)
run(cv, metric=rmsle, plotResults=FALSE, logged=FALSE)
