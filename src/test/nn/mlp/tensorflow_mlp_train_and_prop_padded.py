import numpy as np
import os

os.environ["CUDA_VISIBLE_DEVICES"] = "1"

for layer0_size in [224, 228, 230, 231, 234]:
    import tensorflow_mlp
    for n_inputs in [416]:
        print("layer0_size = " + str(layer0_size) + ", n_inputs = " + str(n_inputs))
        tensorflow_mlp.train_and_forward_propagate([layer0_size, 32], n_inputs)

