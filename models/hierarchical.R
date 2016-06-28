library(data.table)
library(lme4)
library(sqldf)
sapply(dir("R", full.names = TRUE), source)


# perl command line to sample file:
# perl -ne 'print if (rand() < .0001)' train.csv > train_sample.csv
# perl -ne 'print if (rand() < .001)' train.csv > train_sample_75k.csv

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train_sample_75k.csv", col.names=colnames(cnames),
              colClasses=c("numeric", rep("factor", 5), rep("numeric", 5)))
train_wk9 = read.csv.sql("data/train.csv", sql="select * from file where Semana == 9")
test = fread("data/test.csv")



model = "randomEffects_product"

# model = lmer(Demanda_uni_equil ~ Semana + (1|Cliente_ID:Ruta_SAK:Agencia_ID) + (1|Producto_ID), data=train)
model_cv = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Producto_ID), data=train[Semana != 9, ])
preds = exp(predict(model_cv, train_wk9, allow.new.levels=TRUE)) - 1
score = rmsle(preds, train_wk9$Demanda_uni_equil)
write.csv(preds, paste0("data/models/cv_", model, "_", score, ".csv"), row.names=FALSE)

model_full = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Producto_ID), data=train)
preds = exp(predict(model_full, test, allow.new.levels=TRUE)) - 1
out = data.frame(test$id, preds)
colnames(out) = c("id", "Demanda_uni_equil")
out$id = as.character(out$id)
write.csv(out, file=paste0("data/models/test_", model, ".csv"), row.names=FALSE)



model = "randomEffects_store"

# model = lmer(Demanda_uni_equil ~ Semana + (1|Cliente_ID:Ruta_SAK:Agencia_ID) + (1|Producto_ID), data=train)
model_cv = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Cliente_ID), data=train[Semana != 9, ])
preds = exp(predict(model_cv, train_wk9, allow.new.levels=TRUE)) - 1
score = rmsle(preds, train_wk9$Demanda_uni_equil)
write.csv(preds, paste0("data/models/cv_", model, "_", score, ".csv"), row.names=FALSE)

model_full = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Producto_ID), data=train)
preds = exp(predict(model_full, test, allow.new.levels=TRUE)) - 1
out = data.frame(test$id, preds)
colnames(out) = c("id", "Demanda_uni_equil")
out$id = as.character(out$id)
write.csv(out, file=paste0("data/models/test_", model, ".csv"), row.names=FALSE)



model = "hierarchical_store"

# model = lmer(Demanda_uni_equil ~ Semana + (1|Cliente_ID:Ruta_SAK:Agencia_ID) + (1|Producto_ID), data=train)
model_cv = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Cliente_ID), data=train[Semana != 9, ])
preds = exp(predict(model_cv, train_wk9, allow.new.levels=TRUE)) - 1
score = rmsle(preds, train_wk9$Demanda_uni_equil)
write.csv(preds, paste0("data/models/cv_", model, "_", score, ".csv"), row.names=FALSE)

model_full = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Producto_ID), data=train)
preds = exp(predict(model_full, test, allow.new.levels=TRUE)) - 1
out = data.frame(test$id, preds)
colnames(out) = c("id", "Demanda_uni_equil")
out$id = as.character(out$id)
write.csv(out, file=paste0("data/models/test_", model, ".csv"), row.names=FALSE)
