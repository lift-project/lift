#!/usr/bin/env python

import numpy as np


x = np.array([-1, +1, +1, -1])
y = np.array([-1, -1, +1, +1])

# z = np.arctan2(y, x) * 180 / np.pi
z = np.arctan2(y, x)


r = list(map(str, z) )

print(" ".join(r) )







