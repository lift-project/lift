#!/usr/bin/env python

import numpy as np


y = np.interp([1., 1.5, 2.72], [1.,2.,3.], [3.,2.,0.]) 

z = list(map(str, y) )

print(" ".join(z) )







