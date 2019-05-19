#!/usr/bin/env python

import numpy as np


y = np.fmin([1.,9.,3.,4.], [2.,3.,4.,5.]) 

z = list(map(str, y) )

print(" ".join(z) )







