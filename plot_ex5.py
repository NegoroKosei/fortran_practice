import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

# .datファイルからデータを読み込む
df = pd.read_csv('ex3_file/output001.d', delim_whitespace=True, header=None)

# 2列目(反復回数)をx、1列目(解)をyとして指定,yは解1との差の絶対値を見る
x = df.iloc[:, 1]
y = np.abs(df.iloc[:, 0] - 1)

# グラフの作成
plt.plot(x, y, marker='o')
plt.xlabel('times')
plt.ylabel('ans')
plt.title('Newton')
# 対数軸の設定
plt.yscale('log')
# 両方のグリッド線を引く
plt.grid(True, which="both", ls="--")
plt.show()

