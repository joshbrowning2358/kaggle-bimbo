library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("../Model Framework/Cross-Validation/crossValidation/R/", full.names = TRUE),
       source)

# cnames = fread("data/train.csv", nrow=0)
# train = fread("data/train_sample_75k.csv", col.names=colnames(cnames))
train = fread("train_complete_sample_45k.csv")

predModel = function(model, X){
    X = merge(X, model[[1]], by=c("Cliente_ID", "Semana"), all.x=TRUE)
    X = merge(X, model[[2]], by=c("Ruta_SAK", "Semana"), all.x=TRUE)
    X = merge(X, model[[3]], by=c("Agencia_ID", "Semana"), all.x=TRUE)
    X = merge(X, model[[4]], by=c("Canal_ID", "Semana"), all.x=TRUE)
    X[is.na(groupMean), groupMean := groupMean2]
    X[is.na(groupMean), groupMean := groupMean3]
    X[is.na(groupMean), groupMean := groupMean4]
    X$groupMean
}
fitModel = function(X, y){
    X[, y := y]
    list(X[, list(groupMean = mean(y)), by=c("Cliente_ID", "Semana")],
         X[, list(groupMean2 = mean(y)), by=c("Ruta_SAK", "Semana")],
         X[, list(groupMean3 = mean(y)), by=c("Agencia_ID", "Semana")],
         X[, list(groupMean4 = mean(y)), by=c("Canal_ID", "Semana")])
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = train[1, ],
                              cvIndices = train$Producto_ID)
summary(productoAvg)
run(productoAvg, metric=RMSLE)
