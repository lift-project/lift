"""
A pooling network implementation example using TensorFlow library.
This example is using the MNIST database of handwritten digits
(http://yann.lecun.com/exdb/mnist/)

Based on Aymeric Damien's project (https://github.com/aymericdamien/TensorFlow-Examples/)
"""

import tensorflow as tf
import json
import os
import numpy as np

# Import MNIST data
from tensorflow.examples.tutorials.mnist import input_data


class PoolNet:
    """
    A pooling network.
    """
    verbose = True

    @staticmethod
    def maxpool2d(x, k=2):
        """
        MaxPool2D wrapper
        :param x:
        :param k:
        :return:
        """
        return tf.nn.max_pool(x, ksize=[1, k, k, 1], strides=[1, k, k, 1],
                              padding='VALID')

    @staticmethod
    def pool_net(x, kernel_shape, image_shape):
        # Reshape input picture
        """

        :param x: 
        :param kernel_shape: 
        :param image_shape: 
        :return: 
        """
        x = tf.reshape(x, shape=[-1, image_shape[0], image_shape[1], image_shape[2]])
        # Max Pooling (down-sampling)
        return PoolNet.maxpool2d(PoolNet.maxpool2d(x, k=2),
                                 k=kernel_shape[0])

    def __init__(self, kernel_shape, image_shape, bogus):
        self.bogus = bogus
        if not self.bogus:
            self.mnist = input_data.read_data_sets("/tmp/data/", one_hot=True)

        # Parameters

        # Network Parameters
        self.kernel_shape = kernel_shape  # (10, 10)
        self.image_shape = image_shape
        self.input_len = self.image_shape[0] * self.image_shape[1]  # MNIST data input (img shape: 28*28)

        # tf Graph input
        self.x = tf.placeholder(tf.float32, [None, self.input_len, self.image_shape[2]])

        # Construct model
        self.pred = PoolNet.pool_net(self.x, self.kernel_shape, self.image_shape)

        # Initializing the variables
        self.init = tf.global_variables_initializer()

        # Session configuration
        self.config = tf.ConfigProto(device_count={'GPU': 0})

        # Get experiment directory name
        self.dir_name = PoolNet.get_dir_name(self.kernel_shape, self.image_shape)
        # Create directory
        if not os.path.isdir(self.dir_name):
            os.mkdir(self.dir_name)

    @staticmethod
    def get_dir_name(kernel_shape, image_shape):
        """
        Forms a directory name for a given experiment.
        :param image_shape: 
        :param kernel_shape:
        :return:
        """
        if os.environ["LIFT_NN_CAFFE_HARNESS"] is None:
            raise EnvironmentError("LIFT_NN_CAFFE_HARNESS is not set!")
        return os.path.join(os.environ["LIFT_NN_CAFFE_HARNESS"],
                            "experiment.pool." + str(kernel_shape[0]) + "." + str(image_shape[0]))

    def fprop(self, n_batches, n_inputs):
        """
        Forward-propagation; saves input samples and classification results as files.
        :param n_inputs:
        :param n_batches:
        """
        test_images = np.empty([n_batches, n_inputs,
                                self.image_shape[0], self.image_shape[1], self.image_shape[2]])
        test_results = np.empty([n_batches, n_inputs,
                                 int((self.image_shape[0] / 2) / self.kernel_shape[0]),
                                 int((self.image_shape[1] / 2) / self.kernel_shape[1]),
                                 self.image_shape[2]])
        # test_targets = np.empty([n_batches, n_inputs, 10])

        input_len = self.image_shape[0] * self.image_shape[1] * self.image_shape[2]
        x = tf.placeholder("float", [None, input_len])

        for batch_no in np.arange(0, n_batches):
            if not self.bogus:
                test_batch_images_flat, _ = self.mnist.test.next_batch(n_inputs)
            else:
                test_batch_images_flat = \
                    np.random.normal(size=(n_inputs, self.image_shape[0] * self.image_shape[1] * self.image_shape[2]))

            test_images[batch_no] = np.reshape(test_batch_images_flat,
                                               [-1, self.image_shape[0], self.image_shape[1], self.image_shape[2]])

            print("Forward-propagating...")

            # Construct model
            pred = self.pool_net(x, self.kernel_shape, self.image_shape)

            # Launch the graph
            with tf.Session(config=self.config) as sess:
                sess.run(self.init)
                # Produce outputs
                test_results[batch_no] = sess.run([pred], feed_dict={x: test_batch_images_flat})[0]

        # Save test images
        json_string = json.dumps(test_images.astype(np.float32).tolist())
        with open(self.dir_name + '/test_images_n' + str(n_inputs) + '.json', 'w') as outfile:
            outfile.write(json_string)
            outfile.close()
        print("Saved (" + str(n_inputs) + ") images, shape: ", end='')
        print(test_images.shape)

        # Save Tensorflow's forward propagation results into a JSON file
        json_string = json.dumps(test_results.astype(np.float32).tolist())
        with open(self.dir_name + '/test_tf_results_n' + str(n_inputs) + '.json', 'w') as outfile:
            outfile.write(json_string)
            outfile.close()
        if self.verbose:
            print("Saved results, shape: ", end='')
            print(test_results.shape)

        # Print results
        if self.verbose:
            np.set_printoptions(threshold=np.inf, suppress=True)
            input_no = 0
            batch_no = 0
            print("Inputs[" + str(batch_no) + "][" + str(input_no) + "]:")
            print(test_images[batch_no][input_no].sum())
            print("Output[" + str(batch_no) + "][" + str(input_no) + "]:")
            print(test_results[0][batch_no][input_no].shape)
            print(test_results[0][batch_no][input_no])
            # print("Output[" + str(batch_no) + "][0:" + str(input_no + 1) + "] maxed:")
            # print([list(decision).index(max(decision)) for decision in test_results[batch_no][:input_no + 1]])
            # print("Correct[" + str(batch_no) + "][0:" + str(input_no + 1) + "]:")
            # print([list(decision).index(max(decision)) for decision in test_targets[batch_no][:input_no + 1]])
