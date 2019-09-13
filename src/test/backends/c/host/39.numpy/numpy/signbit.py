#!/usr/bin/env python

import numpy as np

y = np.signbit([-1.,2.,-3.,4., 0]) 

z = list(map(str, list(map(int, y) ) ) )

print(" ".join(z) )







