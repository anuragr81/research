from pprint import pprint
import pandas as pd
import numpy as np

#import matplotlib.pyplot as plt
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


def get_changes (x):
	res = []
	for i,y in enumerate(x):
		if i >0:
			res.append(float(x[i])/float(x[i-1]))
	return res

kminp = []
# dried_canned_veg has 666.67
# dried_canned_fish has 2000.00
# wheat has 2008:1250, 2012: 1200
# othervegstarch [666.666666666667, 360.0]

ignored_items = ['dried_canned_veg','pasta','matches','batteries','sugarcane',\
	'dried_canned_fish','cigarettes','wheat','othervegstarch','firewood','charcoal','bunscakes','milling',\
	'kerosene']
pdat = pd.read_csv('c:/temp/pdat.csv').dropna()
itemslist = (list(set(pdat.shortname) - set(ignored_items)))
itemnames={}
for i,sh in enumerate(itemslist):
	df = pdat[pdat.shortname==sh]
	df = df.sort_values('tag')

	if len(list(df.price))<4:
		print(list(df.price))
		raise ValueError("Problem with %s " % sh)

	changes = get_changes(list(df.price))
	if not changes or len(changes)<3:
		print(list(df.price))
		raise ValueError ("Problem")
	kminp.append(changes)
	itemnames[i]=sh




# 5 is ideal
"""
For 5:
{0: ['potatoes',
     'brews',
     'tea',
     'beef',
     'pulses',
     'canned_milk',
     'goat',
     'millet_grain',
     'eggs',
     'salt',
     'sugar',
     'onion',
     'rice_paddy',
     'bread',
     'cooking_oil'],
 1: ['cassava_flour', 'yam', 'banana_green', 'millet_flour'],
 2: ['cassava_fresh'],
 3: ['maize_green',
     'sweet_potato',
     'maize_flour',
     'maize_grain',
     'chicken',
     'rice_husked',
     'fresh_milk',
     'greens'],
 4: ['mangoes', 'peanuts', 'fish_seafood', 'coconut', 'citrus', 'banana_ripe']}


{0: ['beef', 'salt', 'canned_milk', 'brews', 'onion', 'goat', 'pulses'], #Meats
 1: ['cassava_fresh'],
 2: ['rice_husked', # cereals
     'greens',
     'maize_green',
     'maize_grain',
     'maize_flour',
     'fresh_milk'],
 3: ['bread', # sweets and fat
     'cooking_oil',
     'eggs',
     'sugar',
     'tea',
     'chicken',
     'potatoes',
     'sweet_potato',
     'millet_grain',
     'rice_paddy'],
 4: ['yam', 'millet_flour', 'banana_green', 'cassava_flour'], # tubers
 5: ['banana_ripe', 'peanuts', 'citrus', 'coconut', 'mangoes', 'fish_seafood']} # fish and fruits


"""
km = KMeans(
    n_clusters=10, init='random',
    n_init=10, max_iter=1000, 
    tol=1e-04, random_state=0
)

y_km = km.fit (np.array(kminp))
labelconstituents={}
for i,x in enumerate(y_km.labels_):
	if x not in labelconstituents:
		labelconstituents[x]=[itemnames[i]]
	else:
		labelconstituents[x].append(itemnames[i])

print(y_km.inertia_)
pprint(labelconstituents)

