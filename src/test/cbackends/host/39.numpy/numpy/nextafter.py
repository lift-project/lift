#!/usr/bin/env python

import numpy as np


y = np.nextafter([5.,2.,3.,4.], [2,2,4,5]) 

z = list(map(str, y) )

print(" ".join(z) )







