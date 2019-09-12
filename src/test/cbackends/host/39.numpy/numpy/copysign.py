#!/usr/bin/env python

import numpy as np


y = np.copysign([1.,2.,3.,4.], [2,0,-3,10]) 

z = list(map(str, y) )

print(" ".join(z) )







