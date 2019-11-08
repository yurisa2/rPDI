import os
import numpy as np
from PIL import Image

os.chdir("C:/Users/Administrator/Documents/rPDI/artigo/sat")

ndvi_files = os.listdir(
                "ndvi_files_resized/")


no_files = len(ndvi_files)



images = np.empty((no_files, 217, 383))
j = 0

for i in ndvi_files:
    image = np.array(Image.open("ndvi_files_resized/" + i))
    print(i)
    images[j+1] = image

images.shape
