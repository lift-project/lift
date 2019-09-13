#!/usr/bin/env python

import numpy as np


x = 3*np.ones(3)
y = 4*np.ones(3)

z = np.hypot(x,y)

r = list(map(str, z) )

print(" ".join(r) )







