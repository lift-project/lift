#!/usr/bin/env python

import numpy as np


y = np.cross([  [3,5,4],[1,2,3] ],[  [2,7,5],[7,5,3] ] ) 

z = list(map(str, y.flatten()) )

print(" ".join(z) )







