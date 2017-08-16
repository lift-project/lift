"""
A Convolutional Network implementation example using TensorFlow library.
This example is using the MNIST database of handwritten digits
(http://yann.lecun.com/exdb/mnist/)

Based on Aymeric Damien's project (https://github.com/aymericdamien/TensorFlow-Examples/)
"""

import tensorflow as tf
import struct
import os
import numpy as np
import pickle
from enum import Enum
from array import array

# Import MNIST data
# from tensorflow.examples.tutorials.mnist import input_data


def saveToBinary(arr, filename):
    flatten = lambda l: [item for sublist in l for item in sublist]
    # Save to binary file
    dim = len(arr.shape)
    if dim == 1:
        float_array = array('f', arr.astype(np.float16).tolist())
    else:
        if dim == 2:
            float_array = array('f', flatten(
                arr.astype(np.float16).tolist()))
        else:
            if dim == 3:
                float_array = array('f', flatten(flatten(
                    arr.astype(np.float16).tolist())))
            else:
                if dim == 4:
                    float_array = array('f', flatten(flatten(flatten(
                        arr.astype(np.float16).tolist()))))
                else:
                    float_array = array('f', flatten(flatten(flatten(flatten(
                        arr.astype(np.float16).tolist())))))
    with open(filename, "wb") as outfile:
        float_array.tofile(outfile)

class FPropMode(Enum):
    MNIST = 1
    RANDOM = 2
    RESTORE = 3


class CNN:
    """
    Convolutional Neural Network. Training and forward-propagation
    """
    verbose = True

    # Create some wrappers for simplicity
    @staticmethod
    def conv2d(x, w, b, strides=1):
        """
        Conv2D wrapper, with bias and relu activation
        :param x:
        :param w:
        :param b:
        :param strides:
        :return:
        """
        x = tf.nn.conv2d(x, w, strides=[1, strides, strides, 1], padding='VALID')  # padding='SAME')
        x = tf.nn.bias_add(x, b)
        return tf.nn.relu(x)

    @staticmethod
    def maxpool2d(x, k=2):
        """
        MaxPool2D wrapper
        :param x:
        :param k:
        :return:
        """
        return tf.nn.max_pool(x, ksize=[1, k, k, 1], strides=[1, k, k, 1],
                              padding='VALID')  # padding='SAME')

    @staticmethod
    def conv_net_full(x, weights, biases, image_shape):
        # Reshape input picture
        """
        Full CNN: 2 conv layers + 2 mlp layers
        :param x: 
        :param weights: 
        :param biases: 
        :param image_shape: 
        :return: 
        """
        x = tf.reshape(x, shape=[-1, image_shape[0], image_shape[1], image_shape[2]])

        # Convolution Layer
        conv1 = CNN.conv2d(x, weights['wconv1'], biases['bconv1'])
        # Max Pooling (down-sampling)
        # onv1 = CNN.maxpool2d(conv1, k=2)

        # Convolution Layer
        conv2 = CNN.conv2d(conv1, weights['wconv2'], biases['bconv2'])
        # Max Pooling (down-sampling)
        # conv2 = CNN.maxpool2d(conv2, k=2)

        # Fully connected layer
        # Reshape conv2 output to fit fully connected layer input
        fc1 = tf.reshape(conv2, [-1, weights['wmlp1'].get_shape().as_list()[0]])
        fc1 = tf.add(tf.matmul(fc1, weights['wmlp1']), biases['bmlp1'])
        fc1 = tf.nn.relu(fc1)
        # Apply Dropout
        # fc1 = tf.nn.dropout(fc1, dropout)

        # Output, class prediction
        out = tf.add(tf.matmul(fc1, weights['wout']), biases['bout'])
        out = tf.nn.relu(out)
        return fc1

    @staticmethod
    def conv_net(x, weights, biases, image_shape):
        """
        Truncated CNN: only 2 conv layers
        :param image_shape: 
        :param x:
        :param weights:
        :param biases:
        :return:
        """
        # Reshape input picture
        x = tf.reshape(x, shape=[-1, image_shape[0], image_shape[1], 1])

        # Convolution Layer
        conv1 = CNN.conv2d(x, weights['wconv1'], biases['bconv1'])
        # Max Pooling (down-sampling)
        # conv1 = CNNTrainer.maxpool2d(conv1, k=2)

        # Convolution Layer
        conv2 = CNN.conv2d(conv1, weights['wconv2'], biases['bconv2'])
        # Max Pooling (down-sampling)
        # conv2 = CNNTrainer.maxpool2d(conv2, k=2)

        # Fully connected layer
        # Reshape conv2 output to fit fully connected layer input
        # fc1 = tf.reshape(conv2, [-1, weights['wmlp1'].get_shape().as_list()[0]])
        # fc1 = tf.add(tf.matmul(fc1, weights['wmlp1']), biases['bmlp1'])
        # fc1 = tf.nn.relu(fc1)
        # Apply Dropout
        # fc1 = tf.nn.dropout(fc1, dropout)

        # Output, class prediction
        # out = tf.add(tf.matmul(fc1, weights['wout']), biases['bout'])
        # return out
        return conv2

    def __init__(self, n_kernels, kernel_shape, kernel_stride, image_shape, mlp_size_l2, mlp_size_l3):

        # Parameters
        self.learning_rate = 0.001
        self.training_iters = 200000
        # self.batch_size = 128
        self.display_step = 10

        # Network Parameters
        self.n_kernels = n_kernels  # [16, 32]
        self.kernel_shape = kernel_shape  # (10, 10)
        self.kernel_stride = kernel_stride
        self.image_shape = image_shape
        self.input_len = self.image_shape[0] * self.image_shape[1] * self.image_shape[2]
        self.mlp_size_l2 = mlp_size_l2
        self.n_classes = mlp_size_l3  # MNIST total classes (0-9 digits)
        self.dropout = 0.75  # Dropout, probability to keep units

        # tf Graph input
        self.x = tf.placeholder(tf.float32, [None, self.input_len])
        self.y = tf.placeholder(tf.float32, [None, self.n_classes])
        self.keep_prob = tf.placeholder(tf.float32)  # dropout (keep probability)

        # Session configuration
        self.config = tf.ConfigProto()
        self.config.gpu_options.allow_growth = True
        # log_device_placement=True)

        # Get experiment directory name
        self.inputs_path = CNN.get_inputs_path(self.image_shape[0])
        self.results_path = CNN.get_results_path(self.n_kernels, self.kernel_shape, self.image_shape)
        # Create directory
        if not os.path.isdir(self.results_path):
            os.mkdir(self.results_path)
        if not os.path.isdir(self.inputs_path):
            os.mkdir(self.inputs_path)

        # Store layers weight & bias
        with tf.device('/gpu:0'):
            # self.weights = {
            #     'wconv1': tf.Variable(tf.random_normal([self.n_kernels[0], self.kernel_shape[0], self.kernel_shape[1],
            #                                             self.image_shape[2]])),
            #     'wconv2': tf.Variable(
            #         tf.random_normal([self.n_kernels[1], self.kernel_shape[0], self.kernel_shape[1],
            #                           self.n_kernels[0]])),
            #     'wmlp1': tf.Variable(
            #         tf.random_normal([self.n_kernels[1] *
            #                           (self.image_shape[0] - (self.kernel_shape[0] - self.kernel_stride[0]) * 2) *
            #                           (self.image_shape[1] - (self.kernel_shape[1] - self.kernel_stride[1]) * 2),
            #                           self.mlp_size_l2])),
            #     'wout': tf.Variable(tf.random_normal([self.mlp_size_l2, self.n_classes]))
            # }
            # self.trained_weights = None
            #
            # self.biases = {
            #     'bconv1': tf.Variable(tf.random_normal([self.n_kernels[0]])),
            #     'bconv2': tf.Variable(tf.random_normal([self.n_kernels[1]])),
            #     'bmlp1': tf.Variable(tf.random_normal([self.mlp_size_l2])),
            #     'bout': tf.Variable(tf.random_normal([self.n_classes]))
            # }
            self.trained_biases = None

            # Construct model
            # self.pred = CNN.conv_net_full(self.x, self.weights, self.biases, self.image_shape)  # , keep_prob)
            #
            # # Define loss and optimizer
            # self.cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=self.pred, labels=self.y))
            # self.optimizer = tf.train.AdamOptimizer(learning_rate=self.learning_rate).minimize(self.cost)
            #
            # # Evaluate model
            # self.correct_pred = tf.equal(tf.argmax(self.pred, 1), tf.argmax(self.y, 1))
            # self.accuracy = tf.reduce_mean(tf.cast(self.correct_pred, tf.float32))
            #
            # # Initializing the variables
            # self.init = tf.global_variables_initializer()

    @staticmethod
    def get_results_path(n_kernels, kernel_shape, image_shape):
        """
        Forms a directory name for a given experiment parameters and outputs.
        :param image_shape: 
        :param n_kernels:
        :param kernel_shape:
        :return:
        """
        if os.environ["LIFT_NN_RESOURCES"] is None:
            raise EnvironmentError("LIFT_NN_RESOURCES is not set!")
        return os.path.join(os.environ["LIFT_NN_RESOURCES"],
                            "experiment.cnn." + str(n_kernels[1]) + "." + str(kernel_shape[0]) + "." +
                            str(image_shape[0]))

    @staticmethod
    def get_inputs_path(image_shape):
        """
        Forms a directory name for a given experiment inputs.
        :param image_shape: 
        :param n_kernels:
        :param kernel_shape:
        :return:
        """
        if os.environ["LIFT_NN_RESOURCES"] is None:
            raise EnvironmentError("LIFT_NN_RESOURCES is not set!")
        return os.path.join(os.environ["LIFT_NN_RESOURCES"],
                            "experiment.cnn.inputs." + str(image_shape))

    @staticmethod
    def restore(n_kernels, kernel_shape, image_shape):
        """
        Restore data if requested and possible.
        :return:
        """
        results_path = CNN.get_results_path(n_kernels, kernel_shape, image_shape)
        if os.path.isfile(os.path.join(results_path, "pickled_acnn.p")):
            return pickle.load(open(os.path.join(results_path, "pickled_acnn.p"), "rb"))
        else:
            return None

    def train(self):
        """
        Trains the network
        """
        # Load the data
        self.mnist = None # input_data.read_data_sets("/tmp/data/", one_hot=True)

        # Launch the graph
        with tf.device('/gpu:0'):
            with tf.Session(config=self.config) as sess:
                sess.run(self.init)
                step = 1
                # Keep training until reach max iterations
                acc = 0
                # while step * batch_size < training_iters:
                while acc < 0.2:
                    batch_x, batch_y = self.mnist.train.next_batch(self.batch_size)
                    # Run optimization op (backprop)
                    sess.run(self.optimizer, feed_dict={self.x: batch_x, self.y: batch_y,
                                                        self.keep_prob: self.dropout})
                    if step % self.display_step == 0:
                        # Calculate batch loss and accuracy
                        loss, acc = sess.run([self.cost, self.accuracy], feed_dict={self.x: batch_x,
                                                                                    self.y: batch_y})
                        print("Iter " + str(step * self.batch_size) + ", Minibatch Loss= " +
                              "{:.6f}".format(loss) + ", Training Accuracy= " +
                              "{:.5f}".format(acc))
                    step += 1
                print("Optimization Finished!")

                # Calculate accuracy for 256 mnist test images
                print("Testing Accuracy:",
                      sess.run(self.accuracy, feed_dict={self.x: self.mnist.test.images[:256],
                                                         self.y: self.mnist.test.labels[:256],
                                                         self.keep_prob: 1.}))
                # Backup params
                self.trained_weights = sess.run(self.weights)
                self.trained_biases = sess.run(self.biases)

            trained_params = {}
            for weight_key in self.trained_weights:
                if len(self.trained_weights[weight_key].shape) == 2:
                    # MLP weights
                    trained_params[weight_key] = self.trained_weights[weight_key].transpose()
                else:
                    # Convolutional weights
                    trained_params[weight_key] = self.trained_weights[weight_key]
            trained_params = {**trained_params, **self.trained_biases}

            for param_name in trained_params:
                saveToBinary(trained_params[param_name], self.results_path + '/' + param_name + '.binary')
                # json_string = json.dumps(trained_params[param_name].astype(np.float16).tolist())
                # print(trained_params[param_name].shape)
                # with open(self.results_path + '/' + param_name + '.json', 'w') as outfile:
                #     outfile.write(json_string)
                #     outfile.close()
                print("Saved param \"" + param_name + "\"")

    def train_bogus(self):
        """
        Generate random weights instead of training.
        """
        self.trained_weights = {
            'wconv1': TODO: np.random.uniform
                np.random.normal(size=(self.kernel_shape[0], self.kernel_shape[1],
                                             self.image_shape[2], self.n_kernels[0])).astype(dtype=np.float32),
            'wconv2': np.random.normal(size=(self.kernel_shape[0], self.kernel_shape[1],
                                             self.n_kernels[0], self.n_kernels[1])).astype(dtype=np.float32),
            'wmlp1': np.random.normal(
                size=(self.n_kernels[1] *
                      (self.image_shape[0] - (self.kernel_shape[0] - self.kernel_stride[0]) * 2) *
                      (self.image_shape[1] - (self.kernel_shape[1] - self.kernel_stride[1]) * 2),
                      self.mlp_size_l2)).astype(dtype=np.float32),
            'wout': np.random.normal(size=(self.mlp_size_l2, self.n_classes)).astype(dtype=np.float32)
        }

        self.trained_biases = {
            'bconv1': np.random.normal(size=(self.n_kernels[0])).astype(dtype=np.float32),
            'bconv2': np.random.normal(size=(self.n_kernels[1])).astype(dtype=np.float32),
            'bmlp1': np.random.normal(size=self.mlp_size_l2).astype(dtype=np.float32),
            'bout': np.random.normal(size=self.n_classes).astype(dtype=np.float32)
        }
        trained_params = {**self.trained_weights, **self.trained_biases}
        for param_name in trained_params:
            saveToBinary(trained_params[param_name], self.results_path + '/' + param_name + '.binary')
            # json_string = json.dumps(trained_params[param_name].astype(np.float16).tolist())
            # print(trained_params[param_name].shape)
            # with open(self.results_path + '/' + param_name + '.json', 'w') as outfile:
            #     outfile.write(json_string)
            #     outfile.close()
            print("Saved param \"" + param_name + "\"")

    def get_inputs(self, n_batches, n_inputs, mode=FPropMode.MNIST):
        # Create an input dataset
        test_batch_images_flat = None
        filename = self.inputs_path + '/test_images_n' + str(n_inputs) + '.binary'
        if mode is FPropMode.MNIST:
            print("Generating a random subset of MNIST images...")
            for batch_no in np.arange(0, n_batches):
                test_batch_images_flat, _ = self.mnist.test.next_batch(n_inputs)
                print("TODO: flatten batches into a single dataset")
                raise NotImplementedError
        else:
            if mode is FPropMode.RANDOM or not os.path.isfile(filename):
                print("Generating random input...")
                test_batch_images_flat = \
                    np.random.normal(size=(n_batches * n_inputs, self.image_shape[0] * self.image_shape[1] *
                                           self.image_shape[2])).astype(dtype=np.float32)
            else:
                # if mode is FPropMode.RESTORE:
                print("Restoring inputs from a binary file...")
                with open(filename, mode="rb") as infile:
                    test_batch_images_flat = np.asarray(struct.unpack(
                        "f" * n_batches * n_inputs * self.image_shape[0] * self.image_shape[1] *
                        self.image_shape[2], infile.read())).reshape(
                        (n_batches * n_inputs, self.image_shape[0] * self.image_shape[1] * self.image_shape[2]))
        return test_batch_images_flat

    def fprop(self, n_batches, n_inputs, test_batch_images_flat, mode=FPropMode.MNIST):
        """
        Forward-propagation; saves input samples and classification results as files.
        :param mode: 
        :param n_inputs:
        :param n_batches:
        """
        filename = self.inputs_path + '/test_images_n' + str(n_inputs) + '.binary'
        if mode is not FPropMode.RESTORE or not os.path.isfile(filename):
            # Save test images unless they've been successfully restored from files
            test_images = np.reshape(test_batch_images_flat,
                                     [n_batches, n_inputs, self.image_shape[0], self.image_shape[1],
                                      self.image_shape[2]])
            saveToBinary(test_images, filename)
            # json_string = json.dumps(test_images.astype(np.float32).tolist())
            # with open(self.results_path + '/test_images_n' + str(n_inputs) + '.json', 'w') as outfile:
            #     outfile.write(json_string)
            #     outfile.close()
            print("Saved (" + str(test_images.shape[0] * test_images.shape[1]) + ") images, shape: ", end='')
            print(test_images.shape)
            return

        # Convert arrays to tensors
        trained_weights_tensors = {}
        for weight_name in self.trained_weights:
            trained_weights_tensors[weight_name] = \
                tf.Variable(self.trained_weights[weight_name].astype(dtype=np.float32))
            # tf.placeholder(tf.float32,
            #                                                       shape=self.trained_weights[weight_name].shape)
            #tf.convert_to_tensor(self.trained_weights[weight_name].astype(np.float32))

        trained_biases_tensors = {}
        for bias_name in self.trained_biases:
            trained_biases_tensors[bias_name] = \
                tf.Variable(self.trained_biases[bias_name].astype(dtype=np.float32))

        # ---------------------- Forward propagation ---------------------- #

        print("Forward-propagating...")

        # Construct model
        # pred = self.conv_net(x, trained_weights_tensors, trained_biases_tensors, self.image_shape)
        pred = self.conv_net_full(self.x, trained_weights_tensors, trained_biases_tensors, self.image_shape)

        # Initializing the variables
        init = tf.global_variables_initializer()
        # Launch the graph
        with tf.device('/gpu:0'):
            with tf.Session(config=self.config) as sess:
                sess.run(init)
                # Produce outputs
                test_results = sess.run([pred], feed_dict={self.x: test_batch_images_flat})[0]

        # ---------------------- Forward propagation ---------------------- #

        # Save Tensorflow's forward propagation results into a JSON file
        # TODO: test_results = np.reshape(test_results, [n_batches * n_inputs, self.n_classes])
        saveToBinary(test_results, self.results_path + '/test_tf_results_n' + str(n_inputs) + '.binary')

        #test_results = np.reshape(test_results, [n_batches, n_inputs, 400])
        # test_results = np.reshape(test_results, (n_batches, n_inputs, test_results.shape[1],
        #                                          test_results.shape[2], test_results.shape[3]))
        # json_string = json.dumps(test_results.astype(np.float32).tolist())
        # with open(self.results_path + '/test_tf_results_n' + str(n_inputs) + '.json', 'w') as outfile:
        #     outfile.write(json_string)
        #     outfile.close()
        if self.verbose:
            print("Saved results, shape: ", end='')
            print(test_results.shape)
        print(test_results[0])
        quit()

        # Print results
        if self.verbose:
            np.set_printoptions(threshold=np.inf, suppress=True)
            input_no = 0
            batch_no = 0
            # print("Weights['wmlp1'][1]:")
            # print(self.trained_weights['wmlp1'].transpose().shape)
            # print(self.trained_weights['wmlp1'].transpose()[1])
            # print("Biases['bmlp1']:")
            # print(self.trained_biases['bmlp1'].shape)
            # print(self.trained_biases['bmlp1'][1])
            # print("Output[" + str(batch_no) + "][" + str(input_no) + "]:")
            # # print(np.transpose(test_results[batch_no][input_no], (2, 0, 1)).shape)
            # # print(np.transpose(test_results[batch_no][input_no], (2, 0, 1)))
            # print(test_results.shape)
            # print(test_results[0])
            # print()
