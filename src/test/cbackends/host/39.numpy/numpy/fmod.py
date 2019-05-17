#!/usr/bin/env python

import numpy as np


y = np.fmod([1.,2.,3.,4.], [2.,2.,2.,2.]) 

z = list(map(str, y) )

print(" ".join(z) )







