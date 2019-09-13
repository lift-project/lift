#!/usr/bin/env python

import numpy as np


y = np.angle([1+1j, 1, -1j, -1-1j], deg=True) 

z = list(map(str, y) )

print(" ".join(z) )







