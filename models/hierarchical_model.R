library(data.table)
library(lme4)

# perl command line to sample file:
# perl -ne 'print if (rand() < .0001)' train.csv > train_sample.csv
# perl -ne 'print if (rand() < .001)' train.csv > train_sample_75k.csv

cnames = fread("data/train.csv", nrow=0)
train = fread("data/train_sample_7.5M.csv", col.names=colnames(cnames),
              colClasses=c("numeric", rep("factor", 5), rep("numeric", 5)))
test = fread("data/test.csv")

# model = lmer(Demanda_uni_equil ~ Semana + (1|Cliente_ID:Ruta_SAK:Agencia_ID) + (1|Producto_ID), data=train)
model_cv = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Producto_ID), data=train[Semana != 9, ])
preds = exp(predict(model_cv, train[Semana == 9, ], allow.new.levels=TRUE)) - 1
rmsle(preds, train[Semana == 9, Demanda_uni_equil])

model_full = lmer(log(Demanda_uni_equil + 1) ~ Semana + (1|Producto_ID), data=train)
preds = exp(predict(model_full, test, allow.new.levels=TRUE)) - 1
out = data.table(id = test$id, Demanda_uni_equil = preds)
out[, id := as.character(id)]
write.csv(out, file="submissions/hierarchical_simple_model.csv", row.names=FALSE)
