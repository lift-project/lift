#!/usr/bin/env python

import numpy as np


x = np.conj([ 1+1j ,-1+2j,2-3j, -3-4j ]);

y = np.array( (np.real(x), np.imag(x) ) ).flatten()

z = list(map(str, y) )

print(" ".join(z) )







