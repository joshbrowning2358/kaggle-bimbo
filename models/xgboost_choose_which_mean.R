predModel = function(model, X){

}
fitModel = function(X, y){
}
productoAvg = crossValidation(model=list(predict=predModel, fit=fitModel),
                              xTrain = train[Semana != 3, ],
                              yTrain = train[Semana != 3, Demanda_uni_equil],
                              xTest = test,
                              cvIndices = train[Semana != 3, Semana],
                              cvTime=TRUE)
summary(productoAvg)
run(productoAvg, filename="submissions/lm_means_3models", metric=rmsle, logged=TRUE,
    idCol="id", plotResults=TRUE)
