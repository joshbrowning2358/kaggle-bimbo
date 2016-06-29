library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("~/Personal Files/Kaggle/Model Framework/Cross-Validation/crossValidation/R/", full.names = TRUE),
       source)
source("R/error.R")

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train_sample_75k.csv", col.names=colnames(cnames),
              colClasses=c("numeric", rep("factor", 5), rep("numeric", 5)))
train_wk9 = train[Semana == 9, ]
train = train[Semana != 9, ]
test = fread("data/test.csv", nrow=1000)


model = "XGB_product_agency"

X = sparse.model.matrix(Demanda_uni_equil ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + 0,
                        data=train)
X_wk9 = sparse.model.matrix(Demanda_uni_equil ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + 0,
                            data=train_wk9)
X_test = sparse.model.matrix(id ~ Semana + factor(Producto_ID) + factor(Agencia_ID) + 0,
                            data=test)
model_cv = xgboost(data=X, label=train$Demanda_uni_equil, nrounds=20)
pred = predict(model_cv, X_wk9)
score = rmsle(pred, train_wk9$Demanda_uni_equil)
write.csv(pred, paste0("data/models/cv_", model, "_", score, ".csv"), row.names=FALSE)

model_full = xgboost(data=rbind(X, X_wk9),
                     label=c(train$Demanda_uni_equil, train_wk9$Demanda_uni_equil),
                     nrounds=20)
pred = predict(model_cv, X_test)
out = data.frame(test$id, pred)
colnames(out) = c("id", "Demanda_uni_equil")
out$id = as.character(out$id)
write.csv(out, file=paste0("data/models/test_", model, ".csv"), row.names=FALSE)


predModel = function(X, y){
    xgboost(data=matrix(X), label=y, nrounds=10)
}
fitModel = function(model, X){
    predict(model, matrix(X))
}

crossValidation(model=list(predict=predModel, fit=fitModel),
                xTrain = "data.table",
                yTrain = "numeric",
                xTest = "data.table",
                cvIndices = "numeric",
                validationIndices = NULL)
    
