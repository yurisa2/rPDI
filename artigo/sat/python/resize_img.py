import os
from PIL import Image
import numpy as np

os.chdir("/home/yurisa2/lampstack-7.3.7-1/apache2/htdocs/rPDI/artigo/sat")


img_files = os.listdir("rgb_files/")

widths = np.empty(1)
heights = np.empty(1)

for i in img_files:
    im = Image.open("rgb_files/" + i)
    widths = np.append(widths,im.size[0])
    heights = np.append(heights,im.size[1])

size = min(widths).astype("uint8"), min(heights).astype("uint8")

for i in img_files:
    im = Image.open("rgb_files/" + i)
    im = im.resize(size, Image.NEAREST)
    im.save('rgb_files_resized/' + i + '.jpg', "JPEG")
