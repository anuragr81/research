#!/usr/bin/python

from PIL import Image
import numpy as np
import matplotlib.pyplot as plt

img = Image.open("/home/anuragr/test.gif");
iar = np.asarray(img);
plt.imshow(iar)
print(iar)
plt.show()
