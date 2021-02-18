from sklearn.datasets import load_boston #scikit-learn의 datasets에서 sample data import
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

from sklearn.model_selection import train_test_split

from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score


boston = load_boston() # boston dataset load
print(boston.keys()) # 각 key 확인
print(boston.DESCR) # boston datasets description

# 데이터 프레임으로 변환
df = pd.DataFrame(data=boston.data, columns=boston.feature_names)
df['price'] = boston.target
print(df)

df.to_csv("/Users/macbook/boston.csv", sep=',',  na_rep='NaN')   # missing data representation (결측값 표기)