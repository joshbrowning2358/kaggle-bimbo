set.seed(123)
train = fread("data/train.csv")
sample_prods = sample(unique(train$Producto_ID), size=1000)
sample_cliente = sample(unique(train$Cliente_ID), size=1000)

write.csv(train[Cliente_ID %in% sample_cliente & Producto_ID %in% sample_prods, ],
          file="data/train_complete_sample_45k.csv", row.names=FALSE)


set.seed(321)
sample_cliente = sample(unique(train$Cliente_ID), size=10000)

write.csv(train[Cliente_ID %in% sample_cliente, ],
          file="data/train_complete_sample_850k.csv", row.names=FALSE)