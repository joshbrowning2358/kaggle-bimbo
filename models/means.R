library(data.table)
options(scipen=500)

sapply(dir("../Model Framework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")

train = fread("data/train.csv")
test = fread("data/test.csv")

# cnames = fread("data/train.csv", nrow=0)
# train = fread("data/train_complete_sample_45k.csv", col.names=colnames(cnames))
# test = fread("data/test.csv", nrow=1000)


predModel = function(model, X){
    X[, order := 1:.N]
    X = merge(X, model[[1]], by=c("Producto_ID", "Cliente_ID", "Agencia_ID"), all.x=TRUE)
    X = merge(X, model[[2]], by=c("Producto_ID"), all.x=TRUE)
    X[is.na(m1), m1 := m2]
    X[is.na(m1), m1 := model[[3]][["m3"]]]
    X = X[order(order), ]
    X$m1
}
fitModel = function(X, y){
    X[, y := as.numeric(y)]
    list(X[, list(m1 = exp(mean(log(y + 1)))), by=c("Producto_ID", "Cliente_ID", "Agencia_ID")],
         X[, list(m2 = exp(mean(log(y + 1)))), by=c("Producto_ID")],
         X[, list(m3 = exp(mean(log(y + 1))))])
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = test,
                              cvIndices = train$Semana)
summary(productoAvg)
run(productoAvg, filename="hierarchical_log_means", metric=RMSLE, logged=TRUE)
pred <- fread('hierarchical_log_means_full.csv')
pred <- data.table(id = as.character(1:nrow(pred) - 1), Demanda_uni_equil = pred$x)
pred <- data.frame(pred)
write.csv(pred, file='hierarchical_log_means_full.csv', row.names=FALSE)


predModel = function(model, X){
  X[, order := 1:.N]
  X = merge(X, model[[1]], by=c("Producto_ID", "Cliente_ID", "Agencia_ID"), all.x=TRUE)
  X = merge(X, model[[2]], by=c("Producto_ID"), all.x=TRUE)
  X[is.na(m1), m1 := m2]
  X[is.na(m1), m1 := model[[3]][["m3"]]]
  X = X[order(order), ]
  X$m1
}
fitModel = function(X, y){
  X[, y := as.numeric(y)]
  list(X[, list(m1 = exp(median(log(y + 1)))), by=c("Producto_ID", "Cliente_ID", "Agencia_ID")],
       X[, list(m2 = exp(median(log(y + 1)))), by=c("Producto_ID")],
       X[, list(m3 = exp(median(log(y + 1))))])
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train$Demanda_uni_equil,
                              xTest = test,
                              cvIndices = train$Semana)
summary(productoAvg)
run(productoAvg, filename="hierarchical_log_medians", metric=RMSLE, logged=TRUE)
pred <- fread('hierarchical_log_medians_full.csv')
pred <- data.table(id = as.character(1:nrow(pred) - 1), Demanda_uni_equil = pred$x)
write.csv(pred, file='hierarchical_log_medians_full.csv', row.names=FALSE)
