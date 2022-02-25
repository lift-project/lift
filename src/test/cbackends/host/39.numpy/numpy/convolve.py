#!/usr/bin/env python

import numpy as np


y = np.convolve([1.,2.,3.,4.], [2.,3.,4.], 'valid') 

z = list(map(str, y) )

print(" ".join(z) )







