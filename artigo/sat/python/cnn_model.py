import os
import numpy as np
from PIL import Image
import random
from random import randint
from random import sample
import pandas as pd
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Flatten, Conv1D, MaxPooling1D
import matplotlib.pyplot as plt

os.chdir("/home/yurisa2/lampstack-7.3.7-1/apache2/htdocs/rPDI/artigo/sat")

ndvi_files = os.listdir("ndvi_files_resized/")


trained_0 = os.listdir("trained/0/")
trained_1 = os.listdir("trained/1/")

trained_0_df = pd.DataFrame(trained_0)
trained_0_df.describe()

trained_1_df = pd.DataFrame(trained_1)
trained_1_df.describe()

trained_0_string = [t[:34] for t in trained_0]
trained_1_string = [t[:34] for t in trained_1]

trained_0_satno = [t[:4][-2:] for t in trained_0]
trained_1_satno = [t[:4][-2:] for t in trained_1]

trained_0_df["img_name"] = trained_0_string
trained_1_df["img_name"] = trained_1_string

trained_0_df["sat_no"] = trained_0_satno
trained_1_df["sat_no"] = trained_1_satno

trained_0_df = trained_0_df.sample(frac=1).reset_index(drop=True)
trained_1_df = trained_1_df.sample(frac=1).reset_index(drop=True)

trained_0_df["y"] = 0
trained_1_df["y"] = 1

trained_0_df = trained_0_df[0:100]
trained_1_df = trained_1_df[0:100]

trained_full = pd.concat([trained_0_df,trained_1_df])

trained_full = trained_full.sample(frac=1).reset_index(drop=True)
trained_full = trained_full.sample(frac=1).reset_index(drop=True)


trained_full['ndvi'] = trained_full['img_name'] + '_ndvi_norm.jpg.jpg'

x_train_df = trained_full[0:150]['ndvi']
x_test_df = trained_full[151:200]['ndvi']

y_train_df = trained_full[0:150]['y']
y_test_df = trained_full[151:200]['y']

y_train = np.array(y_train_df)
y_test = np.array(y_test_df)

y_train.shape # O ERRO ESTA AQUI
y_train = np.reshape(y_train,(150,1))
y_test.shape # O ERRO ESTA AQUI
y_test = np.reshape(y_test,(49,1))

no_files = len(x_train_df)

x_train = np.empty((no_files, 217, 383))
j = 0

for i in x_train_df:
    image = np.array(Image.open("ndvi_files_resized/" + i))
    x_train[j] = image
    j = j+1


no_files = len(x_test_df)

x_test = np.empty((no_files, 217, 383))
j = 0

for i in x_test_df:
    image = np.array(Image.open("ndvi_files_resized/" + i))
    x_test[j] = image
    j = j+1


y_train_one_hot = to_categorical(y_train)
y_test_one_hot = to_categorical(y_test)

model = Sequential() # Create the architecture

model.add(Conv1D(32, (5), activation='relu', input_shape=(217, 383)))

model.add(MaxPooling1D(pool_size=(2)))

model.add(Conv1D(64, (5), activation='relu'))

model.add(MaxPooling1D(pool_size=(2)))

model.add(Flatten())
model.add(Dense(1000, activation='relu')) #  a layer with 1000 neurons and activation function ReLu
model.add(Dense(2, activation='softmax')) # a layer with 2 output neurons 1 for each label using softmax activation function

model.compile(loss='mean_squared_error', #  loss function used for classes that are greater than 2)
              optimizer='adam',
              metrics=['accuracy'])

hist = model.fit(x_train, y_train_one_hot,
           batch_size=2, epochs=10, validation_split=0.3)

teste = Image.open("python/teste.jpg")
testeArr = np.empty((1, 217, 383))
testeArr[0] = teste

model.predict(testeArr)
