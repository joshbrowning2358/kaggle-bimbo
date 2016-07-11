library(data.table)
library(glmnet)
library(biglm)
options(scipen=500)

sapply(dir("../ModelFramework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")

# train = fread("data/train.csv")
# test = fread("data/test.csv")

# train = fread("data/train_subset_of_features.csv")
# droppedCols = c("Town", "State", "has_multigrain", "has_vanilla", "has_choco", "weight_per_piece", "brand", "weight",
#                 "Venta_hoy", "Dev_uni_proxima", "Dev_proxima")
# train[, c(droppedCols) := NULL]
# test = fread("data/test_subset_of_features.csv")
# test[, c(droppedCols) := NULL]
# gc()

train = fread("data/train_complete_45k_features.csv")
test = fread("data/test_1000_features.csv")


predModel = function(model, X){
  X[is.na(get("mean_Producto_ID<Cliente_ID<Agencia_ID")), c("mean_Producto_ID<Cliente_ID<Agencia_ID") := mean_Producto_ID]
  X[is.na(get("mean_Producto_ID<Cliente_ID<Agencia_ID")), c("mean_Producto_ID<Cliente_ID<Agencia_ID") := mean_]
  X[is.na(mean_Producto_ID), mean_Producto_ID := mean_]
  setnames(X, "mean_Producto_ID<Cliente_ID<Agencia_ID", "x")
  preds = exp(predict(model, newdata=X)) - 1
  preds[preds < 0] = 0
  return(preds)
}
fitModel = function(X, y){
  X[is.na(get("mean_Producto_ID<Cliente_ID<Agencia_ID")), c("mean_Producto_ID<Cliente_ID<Agencia_ID") := mean_Producto_ID]
  X[is.na(get("mean_Producto_ID<Cliente_ID<Agencia_ID")), c("mean_Producto_ID<Cliente_ID<Agencia_ID") := mean_]
  X[is.na(mean_Producto_ID), mean_Producto_ID := mean_]
  #fit = lm(Demanda_uni_equil ~ ., data=X[, c("mean_Producto_ID<Cliente_ID<Agencia_ID", "mean_Producto_ID", "mean_", "Demanda_uni_equil"), with=FALSE])
  setnames(X, "mean_Producto_ID<Cliente_ID<Agencia_ID", "x")
  fit = lm(log(Demanda_uni_equil + 1) ~ log(x + 1), data=X)
  return(fit)
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train[Semana != 3, ],
                              yTrain = train[Semana != 3, Demanda_uni_equil],
                              xTest = test,
                              cvIndices = train[Semana != 3, Semana],
                              cvTime=TRUE)
summary(productoAvg)
run(productoAvg, filename="lasso_hierarchical_means", metric=rmsle, logged=FALSE, plotResults=FALSE)


predModel = function(model, X){
  if(!"Demanda_uni_equil" %in% colnames(X))
    X[, Demanda_uni_equil := 0]
  X[is.na(Demanda_uni_equil), Demanda_uni_equil := 0]
  X = X[, c("x", "mean_Producto_ID", "mean_", "Demanda_uni_equil"), with=FALSE]
  pred1 = exp(predict(model[[1]], newdata=X)) - 1
  pred2 = exp(predict(model[[2]], newdata=X)) - 1
  pred3 = exp(predict(model[[3]], newdata=X)) - 1
  preds = pred3
  preds[X[, !is.na(mean_Producto_ID)]] = pred2
  preds[X[, !is.na(x)]] = pred1
  preds[preds < 0] = 0
  return(preds)
}
fitModel = function(X, y){
  X = X[, c("x", "mean_Producto_ID", "mean_", "Demanda_uni_equil"), with=FALSE]
  mod1 = biglm(log(Demanda_uni_equil + 1) ~ log(x + 1), data=X[!is.na(x), ])
  mod2 = biglm(log(Demanda_uni_equil + 1) ~ log(mean_Producto_ID + 1), data=X[is.na(x) & !is.na(mean_Producto_ID), ])
  mod3 = biglm(log(Demanda_uni_equil + 1) ~ log(mean_), data=X[is.na(x) & is.na(mean_Producto_ID), ])
  return(list(mod1, mod2, mod3))
}
setnames(train, "mean_Producto_ID<Cliente_ID<Agencia_ID", "x")
setnames(test, "mean_Producto_ID<Cliente_ID<Agencia_ID", "x")
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train[Semana != 3, ],
                              yTrain = train[Semana != 3, Demanda_uni_equil],
                              xTest = test,
                              cvIndices = train[Semana != 3, Semana],
                              cvTime=TRUE)
summary(productoAvg)
run(productoAvg, filename="submissions/lm_means_3models", metric=rmsle, logged=TRUE,
    idCol="id", plotResults=FALSE)


train = fread("data/train_complete_45k_features.csv")
test = fread("data/test_1000_features.csv")

predModel = function(model, X){
  if(!"Demanda_uni_equil" %in% colnames(X))
    X[, Demanda_uni_equil := 0]
  X[is.na(Demanda_uni_equil), Demanda_uni_equil := 0]
  X = X[, c("x", "mean_", "Demanda_uni_equil"), with=FALSE]
  pred1 = exp(predict(model[[1]], newdata=X)) - 1
  pred2 = exp(predict(model[[2]], newdata=X)) - 1
  preds = pred2
  preds[X[, !is.na(x)]] = pred1
  preds[preds < 0] = 0
  return(preds)
}
fitModel = function(X, y){
  X = X[, c("x", "mean_", "Demanda_uni_equil"), with=FALSE]
  mod1 = biglm(log(Demanda_uni_equil + 1) ~ log(x + 1), data=X[!is.na(x), ])
  mod2 = biglm(log(Demanda_uni_equil + 1) ~ log(mean_ + 1), data=X[is.na(x), ])
  return(list(mod1, mod2))
}
meanCol = grep("mean_[a-zA-Z]", colnames(train), value=TRUE)
for(col in meanCol){
    cat("Fitting column: ", col, "\n")
    setnames(train, col, "x")
    setnames(test, col, "x")
    productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                                  xTrain = train[Semana != 3, ],
                                  yTrain = train[Semana != 3, Demanda_uni_equil],
                                  xTest = test,
                                  cvIndices = train[Semana != 3, Semana],
                                  cvTime=TRUE)
    run(productoAvg, filename=paste0("testing_", cleanName(col), "_means"), metric=rmsle, logged=TRUE,
        idCol="id", plotResults=FALSE)
    setnames(train, "x", col)
    setnames(test, "x", col)
}