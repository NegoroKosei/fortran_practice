#3Dプロットプログラム

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import animation
from scipy.spatial import KDTree

# ファイルパスの設定
file_path = 'graphene.dat'

# データを格納するリスト
x_data = []
y_data = []
z_data = []  # z軸データのリスト

# ファイルを読み込み、DをEに置換してデータを抽出
with open(file_path, 'r') as file:
    for line in file:
        # DをEに置換
        line = line.replace('D', 'E')
        # 空白で分割してデータを取得
        parts = line.split()
        # x、y、zのデータをリストに追加
        x_data.append(float(parts[0]))  # 1列目のデータ
        y_data.append(float(parts[1]))  # 2列目のデータ
        z_data.append(float(parts[2]))  # 3列目のデータ

# 3Dプロットの作成
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# データのプロット
ax.scatter(x_data, y_data, z_data, marker='o', color='b', s = 0.5)  # sオプションでマーカーサイズを設定

# KDTreeを作成
tree = KDTree(list(zip(x_data, y_data, z_data)))

# 各点から最も近い点を結ぶ線を描画
for x, y, z in zip(x_data, y_data, z_data):
    dist, ind = tree.query([x, y, z], k=4)  # 最も近い3点を探索（自身を含む）
    for d, i in zip(dist[1:], ind[1:]):  # 自身を除く最も近い点を取得
        if d <= 1.5:  # 距離が1.5以下の場合のみ線を描画
            nearest_x, nearest_y, nearest_z = tree.data[i]
            ax.plot([x, nearest_x], [y, nearest_y], [z, nearest_z], color='b', linewidth=0.5)  # 線を描画（色と線幅を設定）

# アスペクト比を設定
ax.set_aspect('auto')

# ラベル設定
ax.set_xlabel('X axis')
ax.set_ylabel('Y axis')
ax.set_zlabel('Z axis')
ax.set_title('Graphene structure')

plt.show()