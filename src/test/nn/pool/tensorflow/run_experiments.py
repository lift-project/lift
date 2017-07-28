import numpy as np
import os
from poolnet import PoolNet
import pickle

os.environ["CUDA_VISIBLE_DEVICES"] = "1"

rerun = True

kernel_sizes = np.arange(4, 64+1, 4)
batches_ns = [2]
inputs_ns = [8]
input_channels_ns = [8]
image_sizes = np.arange(8, 128+1, 8)
for kernel_size in kernel_sizes:
    for image_size in image_sizes:
        for input_channels_n in input_channels_ns:
            # Check if kernel is not too big for the image
            if image_size > kernel_size and image_size % kernel_size == 0:
                print("Creating a PoolNet with following parameters:")
                print("kernel_shape=[" + str(kernel_size) + ", " + str(kernel_size) + "]\n" +
                      "image_shape=[" + str(image_size) + ", " + str(image_size) + ", " + str(input_channels_n) + "]")

                apoolnet = PoolNet(kernel_shape=[kernel_size, kernel_size],
                                   image_shape=[image_size, image_size, input_channels_n],
                                   bogus=True)

                try:
                    for batches_n in batches_ns:
                        for inputs_n in inputs_ns:
                            if rerun or \
                                    not os.path.isfile(apoolnet.dir_name + '/test_images_n' + str(inputs_n) + '.json'):
                                print("Forward-propagating " + str(inputs_n) + " inputs.")
                                apoolnet.fprop(batches_n, inputs_n)
                except ValueError as e:
                    print(str(e))
