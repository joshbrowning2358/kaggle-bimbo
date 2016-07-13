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

setnames(train, gsub("<", "\\.", colnames(train)))
setnames(test, gsub("<", "\\.", colnames(test)))

factorVars = c("Producto_ID", "Agencia_ID", "Town", "brand")
numericVars = c("Semana", "has_vanilla", "has_choco", "mean_", "mean_Producto_ID", "mean_",
                "mean_Producto_ID.Cliente_ID.Agencia_ID", "mean_Town.State", "mean_Canal_ID")
fitModel = function(X, y){
    X[, target := y]
    formula = paste0("target ~ factor(", paste0(factorVars, collapse=") + factor("), ")")
    Xfact = sparse.model.matrix(as.formula(formula), data=X, na.action=na.pass)
    X = cbind(Xfact, Matrix::as.matrix(X[, numericVars, with=FALSE]))
    semana = X[, colnames(X) == "Semana"]
    maxSemana = max(semana)
    folds = list(which(semana < maxSemana), which(semana == maxSemana))
    # Objective of MSE with log(y) <=> RMSE with log(y) <=> RMLSE with y
    xgboost(data=X, label=log(y + 1), nrounds=20, folds=folds,
            params = list(eta=0.1, max_depth=6))
}
predModel = function(model, X){
    X[, target := -9999]
    formula = paste0("target ~ factor(", paste0(factorVars, collapse=") + factor("), ")")
    Xfact = sparse.model.matrix(as.formula(formula), data=X, na.action=na.pass)
    X = cbind(Xfact, Matrix::as.matrix(X[, numericVars, with=FALSE]))
    preds = xgboost::predict(model, X)
    (exp(preds) - 1)
}
cv = crossValidation(model=list(predict=predModel, fit=fitModel),
                    xTrain = train,
                    yTrain = train[, Demanda_uni_equil],
                    xTest = test,
                    cvIndices = train[, Semana])
summary(cv)
run(cv, metric=rmsle, plotResults=FALSE, logged=FALSE)
