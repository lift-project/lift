#!/usr/bin/env python

import numpy as np


y = np.remainder([-7.,2.,3.,-4., 7.], [-3.,3.,-4.,5., -3]) 

z = list(map(str, y) )

print(" ".join(z) )







