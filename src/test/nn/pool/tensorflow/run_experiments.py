import numpy as np
import os
from cnn import CNN
import pickle

os.environ["CUDA_VISIBLE_DEVICES"] = "1"

rerun = False

#kernel_sizes = np.arange(5, 25+1, 5)
kernel_sizes = np.arange(4, 64+1, 4)
kernel_ns_l0 = [16]
kernel_ns_l1 = np.arange(8, 48+1, 4)
batches_ns = [2]
#inputs_ns = np.arange(8, 520+1, 32)
inputs_ns = [8]
#image_sizes = np.arange(16, 512+1, 16)
image_sizes = [8, 16, 32, 64]
# TODO: set kernel size for layer 0
for kernel_n_l0 in kernel_ns_l0:
    for kernel_n_l1 in kernel_ns_l1:
        for kernel_size in kernel_sizes:
            for image_size in image_sizes:
                # Check if kernel is not too big for the image
                if image_size - (kernel_size - 1) * 2 > 0:
                    print("Creating a CNN with following parameters:")
                    print("n_kernels=[" + str(kernel_n_l0) + ", " + str(kernel_n_l1) + "], kernel_shape=[" +
                          str(kernel_size) + ", " + str(kernel_size) + "]")

                    acnn = CNN(n_kernels=[kernel_n_l0, kernel_n_l1],
                               kernel_shape=[kernel_size, kernel_size],
                               image_shape=[image_size, image_size])
                    load = None
                    if not rerun:
                        print("Loading trained parameters from a pickle.")
                        load = CNN.restore(n_kernels=[kernel_n_l0, kernel_n_l1],
                                           kernel_shape=[kernel_size, kernel_size],
                                           image_shape=[image_size, image_size])
                        if load is not None:
                            (acnn.trained_weights, acnn.trained_biases) = load
                    if load is None:
                        if rerun:
                            print("Could not load; retraining parameters.")
                        #acnn.train()
                        acnn.train_bogus()
                        print("Backing up parameters.")
                        pickle.dump((acnn.trained_weights, acnn.trained_biases),
                                    open(os.path.join(acnn.dir_name, "pickled_acnn.p"), "wb"))

                    try:
                        for batches_n in batches_ns:
                            for inputs_n in inputs_ns:
                                if rerun or \
                                        not os.path.isfile(acnn.dir_name + '/test_images_n' + str(inputs_n) + '.json'):
                                    print("Forward-propagating " + str(inputs_n) + " inputs.")
                                    acnn.fprop(batches_n, inputs_n, bogus=True)
                    except ValueError as e:
                        print(str(e))
