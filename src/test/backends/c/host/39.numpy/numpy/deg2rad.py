#!/usr/bin/env python

import numpy as np


y = np.deg2rad([0.,   30.,   60.,   90.,  120.,  150.,  180.,  210.,  240., 270.,  300.,  330.]) 

z = list(map(str, y) )

print(" ".join(z) )







