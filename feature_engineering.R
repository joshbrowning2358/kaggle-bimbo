library(data.table)

sapply(dir("~/Personal Files/Kaggle/ModelFramework/FeatureEngineering/featureEngineering/R/", full.names = TRUE),
       source)

# train = fread("data/train.csv")
# test = fread("data/test.csv")

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train_complete_sample_45k.csv", col.names=colnames(cnames))
test = fread("data/test.csv", nrow=1000)

#######################################################################################################################
#                                Producto Information                                                                 #
#######################################################################################################################

productInfo = fread("data/preprocessed_products.csv")
productInfo[, c("product_name") := NULL]
setnames(productInfo, "ID", "Producto_ID")
train = merge(train, productInfo, by="Producto_ID", all.x=TRUE)
test = merge(test, productInfo, by="Producto_ID", all.x=TRUE)

#######################################################################################################################
#                                Producto Information                                                                 #
#######################################################################################################################

townState = fread("data/town_state.csv")
train = merge(train, townState, by="Agencia_ID", all.x=TRUE)
test = merge(test, townState, by="Agencia_ID", all.x=TRUE)

#######################################################################################################################
#                                Hierarchical Means                                                                   #
#######################################################################################################################

hierarchies = list(c("Producto_ID"), c("Producto_ID", "Cliente_ID", "Agencia_ID"), c("Producto_ID", "brand"),
                   c("Cliente_ID", "Agencia_ID"), c("has_choco"), c("has_choco", "Town"), c("Town", "State"),
                   c("Canal_ID"), c("brand", "has_choco"), "", c("Town", "State", "Canal_ID"), c("Town"), c("State"))
for(h in hierarchies){
    semanasToModel = 3:11
    newFeature = lapply(semanasToModel, function(s){
        groupAvg = train[Semana < s, list(new = mean(Demanda_uni_equil)), by=h]
        groupAvg[, Semana := s]
        groupAvg
    })
    newFeature = do.call("rbind", newFeature)
    setnames(newFeature, "new", paste0("mean_", paste0(h, collapse="<")))
    suppressWarnings({
      if(h == "")
        h = NULL # small hack to get last week's global mean
    })
    train = merge(train, newFeature, by=c(h, "Semana"), all.x=TRUE)
    test = merge(test, newFeature, by=c(h, "Semana"), all.x=TRUE)
}

#######################################################################################################################
#                                Hierarchical Trends                                                                  #
#######################################################################################################################

model = function(d, s){
  fit = lm(Demanda_uni_equil ~ Semana, data=d)
  suppressWarnings({
    predict(fit, newdata=data.frame(Semana=s))
  })
}
for(h in hierarchies){
  semanasToModel = 4:11
  newFeature = lapply(semanasToModel, function(s){
    groupTrend = train[Semana < s, list(new = model(.SD, s)), by=h]
    groupTrend[, Semana := s]
    groupTrend
  })
  newFeature = do.call("rbind", newFeature)
  setnames(newFeature, "new", paste0("trend_", paste0(h, collapse="<")))
  suppressWarnings({
    if(h == "")
      h = NULL # small hack to get last week's global trend
  })
  train = merge(train, newFeature, by=c(h, "Semana"), all.x=TRUE)
  test = merge(test, newFeature, by=c(h, "Semana"), all.x=TRUE)
}

#######################################################################################################################
#                                             Output                                                                  #
#######################################################################################################################

# write.csv(train, "data/train_features.csv", row.names=FALSE)
# write.csv(test, "data/test_features.csv", row.names=FALSE)

write.csv(train, "data/train_complete_45k_features.csv", row.names=FALSE)
write.csv(test, "data/test_1000_features.csv", row.names=FALSE)
