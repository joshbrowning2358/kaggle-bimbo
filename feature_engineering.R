library(data.table)

sapply(dir("~/Personal Files/Kaggle/ModelFramework/FeatureEngineering/featureEngineering/R/", full.names = TRUE),
       source)

# train = fread("data/train.csv")
# test = fread("data/test.csv")

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train_complete_sample_45k.csv", col.names=colnames(cnames))
test = fread("data/test.csv", nrow=1000)

hierarchies = list(c("Producto_ID"))
for(h in hierarchies){
    semanasToModel = 3:11
    newFeature = lapply(semanasToModel, function(s){
        productoAvg = train[Semana < s, list(mu_by_producto = mean(Demanda_uni_equil)),
                            by=]
        productoAvg[, Semana := s]
        productoAvg
    })
    newFeature = do.call("rbind", newFeature)
    train = merge(train, newFeature, by=c("Producto_ID", "Semana"), all.x=TRUE)
    test = merge(test, newFeature, by=c("Producto_ID", "Semana"), all.x=TRUE)
}