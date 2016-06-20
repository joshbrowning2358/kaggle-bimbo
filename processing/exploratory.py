import pandas as pd

pd.read_csv('data/cliente_tabla.csv', nrows=10) # Seems pretty useless...
pd.read_csv('data/producto_tabla.csv', nrows=10) # Maybe useful for text mining techniques
pd.read_csv('data/sample_submission.csv', nrows=10)
pd.read_csv('data/test.csv', nrows=10) # not many columns!
pd.read_csv('data/town_state.csv', nrows=10) # useful to join
pd.read_csv('data/train.csv', nrows=10)

test_data = pd.read_csv('data/test.csv')
grp = test_data.groupby('Semana')
grp.count()

columns = pd.DataFrame({'names': ['Semana', 'Agencia_ID', 'Canal_ID', 'Ruta_SAK', 'Cliente_ID', 'NombreCliente',
                                  'Producto_ID', 'NombreProducto', 'Venta_uni_hoy', 'Venta_hoy', 'Dev_uni_proxima',
                                  'Dev_proxima', 'Demanda_uni_equil'],
                        'type': ['uint8', 'int32', 'uint8', 'int32', 'int64', 'int32', 'int32', 'float32', 'int32'],
                        'english': ['week', 'agency_id', 'sales_channel_id', 'route_id', 'client_id', 'client_name',
                                    'product_id', 'product_name', 'sales_qty', 'sales_amt', 'return_qty', 'return_amt',
                                    'adjusted_demand']
                        })

9 - 3
25759 - 1110
11 - 1
9991 - 1
2015152015 - 26
Couldn't read NombreCliente
49997 - 41
Couldn't read NombreProducto
7200 - 0
647360.0 - 0.0
250000 - 0
130760.0 - 0.0
5000 - 0

for col in columns.names:
    try:
        d = pd.read_csv('data/train.csv', usecols=[col])
        print str(max(d[col])) + ' - ' + str(min(d[col]))
    except:
        print "Couldn't read " + str(col)


train_data = pd.read_csv('data/train.csv',
                         usecols=['Semana', 'Producto_ID', 'Venta_hoy', 'Venta_uni_hoy', 'Dev_uni_proxima',
                                    'Dev_proxima', 'Demanda_uni_equil'],
                         dtype={'Semana': 'int32',
                                'Producto_ID': 'int32',
                                'Venta_hoy': 'float32',
                                'Venta_uni_hoy': 'int32',
                                'Dev_uni_proxima': 'int32',
                                'Dev_proxima': 'float32',
                                'Demanda_uni_equil': 'int32'})
grp = train_data.groupby('Semana')
grp.count()