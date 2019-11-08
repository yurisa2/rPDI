import os
from PIL import Image

os.chdir("C:/Users/Administrator/Documents/rPDI/artigo/sat")


ndvi_files = os.listdir(
                "ndvi_files/")

size = 383, 217

for i in ndvi_files:
    im = Image.open("ndvi_files/" + i)
    im = im.resize(size, Image.NEAREST)
    im.save('ndvi_files_resized/' + i + '.jpg', "JPEG")
