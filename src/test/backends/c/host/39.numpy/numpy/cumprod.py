#!/usr/bin/env python

import numpy as np


y = np.cumprod([1.,2.,3.,4.]) 

z = list(map(str, y) )

print(" ".join(z) )







