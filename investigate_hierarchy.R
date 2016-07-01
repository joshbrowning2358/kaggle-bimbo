library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("~/Personal Files/Kaggle/Model Framework/Cross-Validation/crossValidation/R/", full.names = TRUE),
       source)

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train_sample_75k.csv", col.names=colnames(cnames))

predModel = function(model, X){
    X = merge(X, model, by=c("Cliente_ID", "Semana"), all.x=TRUE)
    X$groupMean
}
fitModel = function(X, y){
    X[, y := y]
    X[, list(groupMean = mean(y)), by=c("Cliente_ID", "Semana")]
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = train[1, ],
                              cvIndices = train$Producto_ID)
summary(productoAvg)
run(productoAvg)
