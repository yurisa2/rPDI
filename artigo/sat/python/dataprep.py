import os
import pandas as pd

trained_0 = os.listdir("C:/Users/Administrator/Documents/rPDI/artigo/sat/trained/0/")
trained_1 = os.listdir("C:/Users/Administrator/Documents/rPDI/artigo/sat/trained/1/")

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
