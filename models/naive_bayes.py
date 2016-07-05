import pandas as pd
import numpy as np
from sklearn.naive_bayes import GaussianNB

import os, sys
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname('.'), '..')))
from context import CrossValidation
from ModelFramework.CrossValidation.python.cross_validation import CrossValidation

import importlib
loader = importlib.machinery.SourceFileLoader('report', '/full/path/report/other_py_file.py')
handle = loader.load_module('report')

header = pd.read_csv('data/train.csv', nrows=0)
data = pd.read_csv('data/train_sample_75k.csv', header=None, names=header.columns)

model = GaussianNB()
X = pd.get_dummies(data[["Producto_ID"]], columns=["Producto_ID"], sparse=True)
model.fit(X.ix[:70000, ], np.array(data.ix[:70000, 'Demanda_uni_equil']))
preds = model.predict(X.ix[70000:, ])
