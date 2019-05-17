#!/usr/bin/env python

import numpy as np


y = np.modf([0.,-1.3,3,2.3]) 

flatten = np.array(y).flatten()

z = list(map(str, flatten) )

print(" ".join(z) )







