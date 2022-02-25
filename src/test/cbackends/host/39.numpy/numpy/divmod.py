#!/usr/bin/env python

import numpy as np


y = np.divmod([4.,5.,6.,7.], [2.,3.,4.,5.]) 

flatten = np.array(y).flatten()

z = list(map(str, flatten) )

print(" ".join(z) )







