#!/usr/bin/env python

import numpy as np


y = np.sum( [[0.,1.], [0.,5.]], axis=0) 

z = list(map(str, y) )

print(" ".join(z) )







