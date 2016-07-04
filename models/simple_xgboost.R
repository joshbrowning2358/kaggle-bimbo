library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("~/Personal Files/Kaggle/ModelFramework/CrossValidation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train.csv", col.names=colnames(cnames),
              colClasses=c("numeric", rep("factor", 5), rep("numeric", 5)))
train_wk9 = train[Semana == 9, ]
# train = fread("data/train_sample_7.5M.csv", col.names=colnames(cnames),
#               colClasses=c("numeric", rep("factor", 5), rep("numeric", 5)))
train = train[Semana != 9, ]
test = fread("data/test.csv")


model = "XGB_product_agency"

data = rbindlist(list(train, train_wk9, test), fill=TRUE)
data[, dummy := 1]
X = sparse.model.matrix(dummy ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + 0,
                        data=data)
X_train = X[1:nrow(train), ]
X_wk9 = X[1:nrow(train_wk9) + nrow(train), ]
X_test = X[1:nrow(test) + nrow(train) + nrow(train_wk9), ]
model_cv = xgboost(data=X_train, label=train$Demanda_uni_equil, nrounds=20)
pred = predict(model_cv, X_wk9)
score = rmsle(pred, train_wk9$Demanda_uni_equil)
write.csv(pred, paste0("data/models/cv_", model, "_", score, ".csv"), row.names=FALSE)

model_full = xgboost(data=rbind(X_train, X_wk9),
                     label=c(train$Demanda_uni_equil, train_wk9$Demanda_uni_equil),
                     nrounds=20)
pred = predict(model_cv, X_test)
out = data.frame(test$id, pred)
colnames(out) = c("id", "Demanda_uni_equil")
out$id = as.character(out$id)
write.csv(out, file=paste0("data/models/test_", model, ".csv"), row.names=FALSE)


fitModel = function(X, y){
    X[, dummy := 1]
    X = sparse.model.matrix(dummy ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + 0,
                            data=X)
    xgboost(data=X, label=y, nrounds=10)
}
predModel = function(model, X){
    X[, dummy := 1]
    X = sparse.model.matrix(dummy ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + 0,
                            data=X)
    xgboost::predict(model, X)
}

cv = crossValidation(model=list(predict=predModel, fit=fitModel),
                    xTrain = rbind(train[Semana != 9, ], train_wk9),
                    yTrain = c(train[Semana != 9, Demanda_uni_equil], train_wk9$Demanda_uni_equil),
                    xTest = test,
                    cvIndices = numeric(0),
                    validationIndices = c(rep(FALSE, nrow(X_train)), rep(TRUE, nrow(train_wk9))))
summary(cv)
run(cv, metric=RMSLE)
