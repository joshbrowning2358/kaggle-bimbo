library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("../Model Framework/Cross-Validation/crossValidation/R/", full.names = TRUE),
       source)

# cnames = fread("data/train.csv", nrow=0)
# train = fread("data/train_sample_75k.csv", col.names=colnames(cnames))
train = fread("data/train_complete_sample_850k.csv")
product_detail = fread("data/preprocessed_products.csv")
product_detail[, product_name := NULL]
train = merge(train, product_detail, by.x="Producto_ID", by.y="ID")

predModel = function(model, X){
    X = merge(X, model[[1]], by=c("Cliente_ID", "Semana"), all.x=TRUE)
    X = merge(X, model[[2]], by=c("Ruta_SAK", "Semana"), all.x=TRUE)
    X = merge(X, model[[3]], by=c("Agencia_ID", "Semana"), all.x=TRUE)
    X = merge(X, model[[4]], by=c("Canal_ID", "Semana"), all.x=TRUE),
    X = merge(X, model[[5]], by=c("Semana"), all.x=TRUE)
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
         X[, list(groupMean4 = mean(y)), by=c("Canal_ID", "Semana")],
         X[, list(groupMean5 = mean(y)), by=c("Semana")])
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = train[1, ],
                              cvIndices = train$Producto_ID)
summary(productoAvg)
run(productoAvg, filename="client_hierarchy", metric=RMSLE)


predModel = function(model, X){
  X = merge(X, model[[1]], by=c("Producto_ID", "Semana"), all.x=TRUE)
  X = merge(X, model[[2]], by=c("Semana"), all.x=TRUE)
  X[is.na(groupMean), groupMean := groupMean2]
  X$groupMean
}
fitModel = function(X, y){
  X[, y := y]
  list(X[, list(groupMean = mean(y)), by=c("Producto_ID", "Semana")],
       X[, list(groupMean2 = mean(y)), by=c("Semana")])
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = train[1, ],
                              cvIndices = train$Producto_ID)
summary(productoAvg)
run(productoAvg, filename="product", metric=RMSLE)


predModel = function(model, X){
  X = merge(X, model[[1]], by=c("Producto_ID", "Semana"), all.x=TRUE)
  X = merge(X, model[[2]], by=c("brand", "Semana"), all.x=TRUE)
  X = merge(X, model[[3]], by=c("Semana"), all.x=TRUE)
  X[is.na(groupMean), groupMean := groupMean2]
  X[is.na(groupMean), groupMean := groupMean3]
  X$groupMean
}
fitModel = function(X, y){
  X[, y := y]
  list(X[, list(groupMean = mean(y)), by=c("Producto_ID", "Semana")],
       X[, list(groupMean2 = mean(y)), by=c("brand", "Semana")],
       X[, list(groupMean3 = mean(y)), by=c("brand", "Semana")])
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = train[1, ],
                              cvIndices = train$Producto_ID)
summary(productoAvg)
run(productoAvg, filename="product_to_brand", metric=RMSLE)


predModel = function(model, X){
  X = merge(X, model[[1]], by=c("Producto_ID", "Semana"), all.x=TRUE)
  X = merge(X, model[[2]], by=c("has_choco", "Semana"), all.x=TRUE)
  X = merge(X, model[[3]], by=c("Semana"), all.x=TRUE)
  X[is.na(groupMean), groupMean := groupMean2]
  X[is.na(groupMean), groupMean := groupMean3]
  X$groupMean
}
fitModel = function(X, y){
  X[, y := y]
  list(X[, list(groupMean = mean(y)), by=c("Producto_ID", "Semana")],
       X[, list(groupMean2 = mean(y)), by=c("has_choco", "Semana")],
       X[, list(groupMean3 = mean(y)), by=c("Semana")])
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = train[1, ],
                              cvIndices = train$Producto_ID)
summary(productoAvg)
run(productoAvg, filename="product_to_choco", metric=RMSLE)