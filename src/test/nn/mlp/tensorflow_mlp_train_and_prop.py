import numpy as np

layer0_size_min = 32
layer0_size_max = 1024
layer0_size_step = 32
n_inputs_min = 32
n_inputs_max = 1900
n_inputs_step = 32
for layer0_size in np.arange(layer0_size_min, layer0_size_max, layer0_size_step):
    for n_inputs in np.arange(n_inputs_min + n_inputs_step, n_inputs_max, n_inputs_step):
        import tensorflow_mlp
        print("layer0_size = " + str(layer0_size) + ", n_inputs = " + str(n_inputs))
        tensorflow_mlp.train_and_forward_propagate([layer0_size, 32], n_inputs)

