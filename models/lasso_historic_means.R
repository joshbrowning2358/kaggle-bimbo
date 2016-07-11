library(data.table)
library(glmnet)
library(biglm)
options(scipen=500)

sapply(dir("../ModelFramework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")

# train = fread("data/train.csv")
# test = fread("data/test.csv")

train = fread("data/train_subset_of_features.csv")
droppedCols = c("Town", "State", "has_multigrain", "has_vanilla", "has_choco", "weight_per_piece", "brand", "weight",
                "Venta_hoy", "Dev_uni_proxima", "Dev_proxima")
train[, c(droppedCols) := NULL]
test = fread("data/test_subset_of_features.csv")
test[, c(droppedCols) := NULL]
gc()

# train = fread("data/train_complete_850k_features.csv")
# test = fread("data/test_1000_features.csv")

predModel = function(model, X){
    meanCols = grep("mean_", colnames(X), value=TRUE)
    trendCols = grep("trend_", colnames(X), value=TRUE)
    sapply(meanCols, function(col) X[is.na(get(col)), c(col) := mean_])
    sapply(trendCols, function(col) X[is.na(get(col)), c(col) := trend_])
    preds = predict(model$glmnet.fit,
                    newx=as.matrix(X[, c(meanCols, trendCols), with=FALSE]),
                    s=model$lambda.1se)
    preds[preds < 0] = 0
    return(preds)
}
fitModel = function(X, y){
    meanCols = grep("mean_", colnames(X), value=TRUE)
    trendCols = grep("trend_", colnames(X), value=TRUE)
    sapply(meanCols, function(col) X[is.na(get(col)), c(col) := mean_])
    sapply(trendCols, function(col) X[is.na(get(col)), c(col) := trend_])
    fit = cv.glmnet(x=as.matrix(X[, c("mean_Producto_ID<Cliente_ID<Agencia_ID", "mean_Producto_ID", "mean_"), with=FALSE]),
                    y=y, alpha=0)
    return(fit)
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train[Semana != 3, ],
                              yTrain = train[Semana != 3, Demanda_uni_equil],
                              xTest = test,
                              cvIndices = train[Semana != 3, Semana],
                              cvTime=TRUE)
summary(productoAvg)
run(productoAvg, filename="lasso_hierarchical_means", metric=rmsle, logged=FALSE)



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
run(productoAvg, filename="lasso_hierarchical_means", metric=rmsle, logged=TRUE)
pred <- fread('lasso_hierarchical_means_full.csv')
pred <- data.table(id = as.character(1:nrow(pred) - 1), Demanda_uni_equil = pred$x)
pred <- data.frame(pred)
write.csv(pred, file='submissions/lasso_hierarchical_means_full.csv', row.names=FALSE)


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
run(productoAvg, filename="submissions/lm_means_3models", metric=rmsle, logged=TRUE, idCol='id')
