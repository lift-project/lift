#!/usr/bin/env python

import numpy as np


y = np.expm1([1.,2.,3.,4.]) 

z = list(map(str, y) )

print(" ".join(z) )







