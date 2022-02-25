#!/usr/bin/env python

import numpy as np


y = np.gradient([1., 2., 4., 7., 11., 16.]) 

z = list(map(str, y) )

print(" ".join(z) )







