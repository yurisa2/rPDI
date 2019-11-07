import os
import pandas as pd
import rasterio
import numpy as np

all_files = os.listdir(
    "C:/Users/Administrator/Documents/rPDI/artigo/sat/represa")

os.chdir("C:/Users/Administrator/Documents/rPDI/artigo/sat/represa")


all_files_df = pd.DataFrame(all_files)

all_files_string = [t[:34] for t in all_files]
all_files_satno = [t[:4][-2:] for t in all_files]
all_files_band = [t[t.find('B'):t.find('.tif')] for t in all_files]

all_files_df["img_name"] = all_files_string
all_files_df["sat_no"] = all_files_satno
all_files_df["band"] = all_files_band

red_band_files_l08 =  all_files_df[(all_files_df.sat_no == "08") & (all_files_df.band == "B4")]
red_band_files_l07 =  all_files_df[(all_files_df.sat_no == "07") & (all_files_df.band == "B3")]
red_band_files_l05 =  all_files_df[(all_files_df.sat_no == "05") & (all_files_df.band == "B3")]
red_band_files_l04 =  all_files_df[(all_files_df.sat_no == "04") & (all_files_df.band == "B3")]

red_band_files = red_band_files_l08
red_band_files = red_band_files.append(red_band_files_l07)
red_band_files = red_band_files.append(red_band_files_l05)
red_band_files = red_band_files.append(red_band_files_l04)

nir_band_files_l08 =  all_files_df[(all_files_df.sat_no == "08") & (all_files_df.band == "B5")]
nir_band_files_l07 =  all_files_df[(all_files_df.sat_no == "07") & (all_files_df.band == "B4")]
nir_band_files_l05 =  all_files_df[(all_files_df.sat_no == "05") & (all_files_df.band == "B4")]
nir_band_files_l04 =  all_files_df[(all_files_df.sat_no == "04") & (all_files_df.band == "B4")]

nir_band_files = nir_band_files_l08
nir_band_files = nir_band_files.append(nir_band_files_l07)
nir_band_files = nir_band_files.append(nir_band_files_l05)
nir_band_files = nir_band_files.append(nir_band_files_l04)


def write_ndvi(red_band, nir_band):

    red = red_band.read(1).astype('float64')
    nir = nir_band.read(1).astype('float64')

    ndvi = np.where(
        (nir + red) == 0., 0,
        (nir - red) / (nir + red))

    ndvi *= 255.0/ndvi.max()

    ndvi = ndvi.astype('uint8')

    ndviImage = rasterio.open('../Output/ultimatenorm.bmp', 'w', driver='BMP',
                              width=red_band.width,
                              height=red_band.height,
                              count=1, crs=red_band.crs,
                              transform=red_band.transform,
                              dtype='uint8')
    ndviImage.write(ndvi, 1)
    ndviImage.close()

    return True


nir_band_files.iloc[45][0]
red_band_files.iloc[45][0]

red_r = rasterio.open(red_band_files.iloc[45][0])  # red raster
nir_r = rasterio.open(nir_band_files.iloc[45][0])  # nir raster

red_file_name = None
for row in nir_band_files.iterrows():
    nir_file_name = row[1][0]
    img_file = row[1]['img_name']
    red_file_name = red_band_files[red_band_files.img_name == img_file][0]
    # print(img_file)

red_file_name.loc[:2]


write_ndvi(red_r, nir_r)
