#!/usr/bin/env python

import numpy as np

y = np.frexp([1.,2.,3.,4.])

zipped = list(zip(y[0], y[1]))

flatten = np.array(zipped).flatten()

z = list(map(str, flatten) )

print(" ".join(z) )







