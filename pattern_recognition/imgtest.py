#!/usr/bin/python

from PIL import Image
import numpy as np
import matplotlib.pyplot as plt

import sys


class InvalidLoadException(Exception):
    pass


class SmallImageException(Exception):
    pass


# the average size of the horizontal bar for detection
bar_ratio = .05;
brightness_median_threshold = .5

image_file_path = sys.argv[1]

img = Image.open(image_file_path);
iar = np.asarray(img);

nrows = iar.shape[0]
ncols = iar.shape[1]

# print "Image loaded of size:",nrows,"(rows)x",ncols,"(cols)"

if nrows * ncols != iar.size:
    raise InvalidLoadException()

# normalize the image
max_data_reciprocal = 1/float(int(max(iar.data).encode('hex'),16))
iar = np.multiply(iar,max_data_reciprocal)

# if the median is higher than
print np.median(iar)
sys.exit(1)
rowdata = iar.tolist()

# normalize the horizontal bar filter
average_horizbar = np.multiply(([1] * int(nrows * bar_ratio)), 1 / float(nrows));

if len(average_horizbar) < 2:
    raise SmallImageException()

print "index,sum"
for i in range(0, nrows):
    row = rowdata[i];
    if not isinstance(row, list):
        TypeError("row not a list")
    #print i, ",", np.mean(np.convolve(row, average_horizbar))
    print row

#plt.imshow(iar)

#print(iar)
#plt.show()
