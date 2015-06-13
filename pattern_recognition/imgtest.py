#!/usr/bin/python

from PIL import Image
import numpy as np
import matplotlib.pyplot as plt

import sys


class InvalidLoadException(Exception):
    pass

class SmallImageException(Exception):
    pass

bar_ratio = .05;

img = Image.open("/home/anuragr/test.gif");
iar = np.asarray(img);

nrows=iar.shape[0]
ncols=iar.shape[1]
#print "Image loaded of size:",nrows,"(rows)x",ncols,"(cols)"

if nrows*ncols != iar.size:
   raise InvalidLoadException()

rowdata = iar.tolist()
average_horizbar = np.multiply(([1]*int(nrows*bar_ratio)),1/float(nrows));

#print average_horizbar
if len(average_horizbar) < 2: 
   raise SmallImageException()

print "index,sum"
for i in range(0,nrows):
   row = rowdata[i];
   if not isinstance(row,list):
       TypeError("row not a list")
   print i,",",np.mean(np.convolve(row,average_horizbar))

#plt.imshow(iar)

#print(iar)
#plt.show()
