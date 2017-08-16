import numpy as np
import os
from cnn import CNN, FPropMode
import pickle

#os.environ["CUDA_VISIBLE_DEVICES"] = "1"

retrain = False
refprop = False
fprop_mode = FPropMode.RESTORE
continueFromPrevious = False

kernel_sizes = np.arange(4, 64 + 1, 4)
kernel_stride = 1
kernel_ns_l0 = [16]
kernel_ns_l1 = np.arange(8, 48 + 1, 4)
mlp_size_l2 = 256
mlp_size_l3 = 10
input_channels = 1
batches_ns = [2]
inputs_ns = [8, 16, 32, 64, 128, 256, 512, 1024, 2048, 2048]
image_sizes = [8, 16, 32, 64, 128, 256, 512, 1024, 2048]
# image_sizes = [128]
inputs = None
previousConfig = None

if continueFromPrevious and\
    os.path.isfile(os.path.join(os.environ["LIFT_NN_RESOURCES"], "last_experiment.p")):
    previousConfig = pickle.load(open(os.path.join(os.environ["LIFT_NN_RESOURCES"], "last_experiment.p"), "rb"))
# TODO: set kernel size for layer 0
    skip = True
else:
    skip = False
for batches_n in batches_ns:
    for inputs_n in inputs_ns:
        for image_size in image_sizes:
            inputs_not_loaded = True
            for kernel_size in kernel_sizes:
                if image_size - (kernel_size - 1) * 2 > 0:
                    # Check if kernel is not too big for the image
                    for kernel_n_l0 in kernel_ns_l0:
                        for kernel_n_l1 in kernel_ns_l1:
                            if skip and \
                                            (batches_n, inputs_n, image_size,
                                             kernel_size, kernel_n_l0, kernel_n_l1) == previousConfig:
                                skip = False
                            if skip:
                                continue
                            # Checkpoint
                            pickle.dump((batches_n, inputs_n, image_size, kernel_size, kernel_n_l0, kernel_n_l1),
                                        open(os.path.join(os.environ["LIFT_NN_RESOURCES"], "last_experiment.p"), "wb"))

                            print("\nCreating a CNN with following parameters:")
                            print("batches_n = " + str(batches_n) + ", inputs_n = " + str(inputs_n) +
                                  ", image_size = " + str(image_size) + "\n" +
                                  "n_kernels=[" + str(kernel_n_l0) + ", " + str(kernel_n_l1) + "], kernel_shape=[" +
                                  str(kernel_size) + ", " + str(kernel_size) + "]")
                            acnn = CNN(n_kernels=[kernel_n_l0, kernel_n_l1],
                                       kernel_shape=[kernel_size, kernel_size],
                                       kernel_stride=[kernel_stride, kernel_stride],
                                       image_shape=[image_size, image_size, input_channels],
                                       mlp_size_l2=mlp_size_l2,
                                       mlp_size_l3=mlp_size_l3)
                            if not retrain and (refprop or
                                                    not os.path.isfile(acnn.results_path + '/test_tf_results_n' +
                                                                               str(inputs_n) + '.binary')):
                                need_to_train_or_fprop = True
                            else:
                                need_to_train_or_fprop = False
                            load = None
                            if need_to_train_or_fprop:
                                print("Loading trained parameters from a pickle.")
                                load = CNN.restore(n_kernels=[kernel_n_l0, kernel_n_l1],
                                                   kernel_shape=[kernel_size, kernel_size],
                                                   image_shape=[image_size, image_size])
                                if load is not None:
                                    (acnn.trained_weights, acnn.trained_biases) = load
                            if need_to_train_or_fprop and load is None:
                                if retrain:
                                    print("Could not load; retraining parameters.")
                                # acnn.train()
                                acnn.train_bogus()
                                print("Backing up parameters.")
                                pickle.dump((acnn.trained_weights, acnn.trained_biases),
                                            open(os.path.join(acnn.results_path, "pickled_acnn.p"), "wb"))

                            try:
                                if refprop or \
                                        not os.path.isfile(acnn.results_path + '/test_tf_results_n' +
                                                                   str(inputs_n) + '.binary'):
                                    print("Forward-propagating " + str(inputs_n) + " inputs.")
                                    if inputs_not_loaded:
                                        print("Getting inputs...")
                                        inputs = acnn.get_inputs(batches_n, inputs_n, mode=fprop_mode)
                                        inputs_not_loaded = False
                                    acnn.fprop(batches_n, inputs_n, inputs, mode=fprop_mode)
                                else:
                                    print("Skipping experiment.")
                            except ValueError as e:
                                print(str(e))