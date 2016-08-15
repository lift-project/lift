import numpy as np
import os

os.environ["CUDA_VISIBLE_DEVICES"] = "1"

layer0_size_start_from = 896 # For continuning an unfinished job
layer0_size_min = 32
layer0_size_max = 1024 + 1
layer0_size_step = 32
n_inputs_start_from = 32 # For continuning an unfinished job
n_inputs_min = 32
n_inputs_max = 1900
#n_inputs_max = 1215
n_inputs_step = 32
for layer0_size in np.arange(max(layer0_size_start_from, layer0_size_min),
    layer0_size_max, layer0_size_step):
    import tensorflow_mlp
    if (layer0_size == layer0_size_start_from):
        n_inputs_start = n_inputs_start_from
    else:
        n_inputs_start = n_inputs_min
    print("layer0_size = " + str(layer0_size) + ", n_inputs = " + str(n_inputs_start))
    tensorflow_mlp.train_and_forward_propagate([layer0_size, 32], n_inputs_start)
    for n_inputs in np.arange(n_inputs_start+32, n_inputs_max, n_inputs_step):
        print("layer0_size = " + str(layer0_size) + ", n_inputs = " + str(n_inputs))
        tensorflow_mlp.forward_propagate([layer0_size, 32], n_inputs)

