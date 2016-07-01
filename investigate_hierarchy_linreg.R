library(data.table)
library(xgboost)
library(Matrix)

sapply(dir("../Model Framework/Cross-Validation/crossValidation/R/", full.names = TRUE),
       source)

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train_sample_35M.csv", col.names=colnames(cnames))
# train = fread("data/train_complete_sample_850k.csv")
product_detail = fread("data/preprocessed_products.csv")
product_detail[, product_name := NULL]
train = merge(train, product_detail, by.x="Producto_ID", by.y="ID")
week_mean = train[, list(week_mean = mean(Dev_uni_proxima), week_cnt = .N), by=Semana]

vars = c("Producto_ID", "Agencia_ID", "Canal_ID", "Ruta_SAK", "Cliente_ID", "brand", "weight", "pieces",
         "weight_per_piece", "has_choco", "has_vanilla", "has_multigrain")
for(var in vars){
  # for Cliente_ID, I want mean of this Cliente_ID for all other obs (in this week) except this one.
  train[, temp_mean := mean(Demanda_uni_equil), by=c(var, "Semana")]
  train[, temp_cnt := .N, by=c(var, "Semana")]
  train[, new_mean := (temp_mean * temp_cnt - Demanda_uni_equil)/(temp_cnt-1)]
  setnames(train, "new_mean", paste0(var, "_mean"))
  train[, c("temp_mean", "temp_cnt") := NULL]
}
train[, Semana_mean := mean(Demanda_uni_equil), by="Semana"]

formula = paste0("Demanda_uni_equil ~ ", paste0(c(vars, "Semana"), "_mean", collapse=" + "), " + 0")
fit = glm(formula = as.formula(formula), data=train)
summary(fit)
paste(gsub("_mean", "", names(rev(sort(fit$coeff)))), collapse=", ")

# 75k: has_choco, Semana, Canal_ID, has_vanilla, Producto_ID, weight, weight_per_piece, Cliente_ID, pieces, Ruta_SAK, brand, Agencia_ID, has_multigrain
# 7.5M: Semana, Producto_ID, Agencia_ID, Cliente_ID, Ruta_SAK, pieces, has_choco, weight_per_piece, has_vanilla, weight, Canal_ID, brand, has_multigrain
# 35M: 
# 45k: has_multigrain, has_choco, pieces, Cliente_ID, Canal_ID, weight, Agencia_ID, weight_per_piece, Ruta_SAK, Producto_ID, has_vanilla, brand, Semana
# 850k: Producto_ID, Agencia_ID, pieces, Cliente_ID, Semana, has_choco, weight_per_piece, Ruta_SAK, Canal_ID, has_vanilla, brand, weight, has_multigrain
