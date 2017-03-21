import numpy as np
import os
from cnn import CNN
import pickle

os.environ["CUDA_VISIBLE_DEVICES"] = "1"

rerun = False

kernel_widths = np.arange(10, 11, 5)  # 5, 25, 5)
kernel_heights = np.arange(10, 11, 5)  # 5, 25, 5)
kernel_ns_l0 = np.arange(16, 17, 4)  # 8, 48, 4)
kernel_ns_l1 = np.arange(32, 33, 4)  # 8, 48, 4)
batches_ns = [2]
inputs_ns = np.arange(8, 9, 32)  # 32, 512, 32)

for kernel_n_l0 in kernel_ns_l0:
    for kernel_n_l1 in kernel_ns_l1:
        for kernel_width in kernel_widths:
            for kernel_heights in kernel_heights:
                print("Creating a CNN with following parameters:")
                print("n_kernels=[" + str(kernel_n_l0) + ", " + str(kernel_n_l1) + "], kernel_shape=[" +
                      str(kernel_width) + ", " + str(kernel_heights) + "]")

                acnn = CNN(n_kernels=[kernel_n_l0, kernel_n_l1],
                           kernel_shape=[kernel_width, kernel_heights])
                load = None
                if not rerun:
                    print("Loading trained parameters from a pickle.")
                    load = CNN.restore(n_kernels=[kernel_n_l0, kernel_n_l1],
                                       kernel_shape=[kernel_width, kernel_heights])
                    if load is not None:
                        (acnn.trained_weights, acnn.trained_biases) = load
                if load is None:
                    if rerun:
                        print("Could not load; retraining parameters.")
                    acnn.train()
                    print("Backing up parameters.")
                    pickle.dump((acnn.trained_weights, acnn.trained_biases),
                                open(os.path.join(acnn.dir_name, "pickled_acnn.p"), "wb"))

                for batches_n in batches_ns:
                    for inputs_n in inputs_ns:
                        print("Forward-propagating " + str(inputs_n) + " inputs.")
                        acnn.fprop(batches_n, inputs_n)
