import pandas as pd
import numpy as np

header = pd.read_csv('data/train.csv', nrows=0)
data = pd.read_csv('data/train_sample_75k.csv', header=None, names=header.columns)

# How well does the leave-one-out cliente/producto/ruta/producto_tipo average predict the observed?
# Hierarchies to consider: Cliente_ID, Producto_ID, Ruta_SAK, Producto_Tipo (mine names)
# End goal: impute missing series values with a weighted average of all 4, maybe choosing weights w/ decision tree

group_var = 'Cliente_ID'
