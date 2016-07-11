library(data.table)
library(glmnet)
library(biglm)
options(scipen=500)

sapply(dir("../ModelFramework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")
source("../ModelFramework/Models/naiveBayesRegression.R")

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


modelCols = c("Town", "Canal_ID", "brand", "has_choco", "Cliente_ID", "Producto_ID",
              "Ruta_SAK", "has_vanilla", "has_multigrain")
for(col in modelCols){
    train[, c(col) := as.factor(get(col))]
    test[, c(col) := as.factor(get(col))]
}
predModel = function(model, X){
    out = predictNaiveBayesRegression(model, X)
    out[out < 0] = 0
    return(out)
}
fitModel = function(X, y){
    fitNaiveBayesRegression(X[, modelCols, with=FALSE], y=X$Demanda_uni_equil,
                            log=FALSE)
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train,
                              yTrain = train[, Demanda_uni_equil],
                              xTest = test,
                              cvIndices = train[, Semana],
                              cvTime=TRUE)
summary(productoAvg)
run(productoAvg, filename="naive_bayes", metric=rmsle, logged=FALSE, plotResults=FALSE)
