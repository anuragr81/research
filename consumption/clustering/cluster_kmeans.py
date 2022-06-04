# -*- coding: utf-8 -*-
"""
Created on Sat Jun  4 09:12:45 2022

@author: anura
"""

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs

from sklearn.cluster import KMeans


def run_example(): 
	# create dataset
	X, y = make_blobs(
	   n_samples=150, n_features=2,
	   centers=3, cluster_std=0.5,
	   shuffle=True, random_state=0
	)

	# plot
	#plt.scatter(
	#   X[:, 0], X[:, 1],
	#   c='white', marker='o',
	#   edgecolor='black', s=50
	#)
	#plt.show()


	km = KMeans(
	    n_clusters=3, init='random',
	    n_init=10, max_iter=300, 
	    tol=1e-04, random_state=0
	)

	#y_km = km.fit_predict(X)
	y_km  = km.fit(X)
	print(y_km.labels_)
	return y_km 



	km = KMeans(
	    n_clusters=3, init='random',
	    n_init=10, max_iter=300, 
	    tol=1e-04, random_state=0
	)



kminp = []
pdat = pd.read_csv('c:/temp/pdat.csv')
for sh in (list(set(pdat.shortname))):
	df = pdat[pdat.shortname==sh]
	df = df.sort_values('tag')
	kminp.append(list(df.price))
	
	#print(sh)
#print(dat.head())
y_km = km.fit (np.array(kminp))
print("DONE")


