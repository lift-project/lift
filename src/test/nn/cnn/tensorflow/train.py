"""
A Convolutional Network implementation example using TensorFlow library.
This example is using the MNIST database of handwritten digits
(http://yann.lecun.com/exdb/mnist/)

Author: Aymeric Damien
Project: https://github.com/aymericdamien/TensorFlow-Examples/
"""

import tensorflow as tf
import json
import os
import numpy as np

# Import MNIST data
from tensorflow.examples.tutorials.mnist import input_data

mnist = input_data.read_data_sets("/tmp/data/", one_hot=True)

# Parameters
learning_rate = 0.001
training_iters = 200000
batch_size = 128
display_step = 10

# Network Parameters
n_kernels = [16, 32]
kernel_shape = (10, 10)
n_input = 784  # MNIST data input (img shape: 28*28)
n_classes = 10  # MNIST total classes (0-9 digits)
dropout = 0.75  # Dropout, probability to keep units

# tf Graph input
x = tf.placeholder(tf.float32, [None, n_input])
y = tf.placeholder(tf.float32, [None, n_classes])
keep_prob = tf.placeholder(tf.float32)  # dropout (keep probability)


# Create some wrappers for simplicity
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


def maxpool2d(x, k=2):
    """
    MaxPool2D wrapper
    :param x:
    :param k:
    :return:
    """
    return tf.nn.max_pool(x, ksize=[1, k, k, 1], strides=[1, k, k, 1],
                          padding='VALID')  # padding='SAME')


def conv_net(x, weights, biases):  # , dropout):
    """
    # Create model
    :param x:
    :param weights:
    :param biases:
    :return:
    """
    # Reshape input picture
    x = tf.reshape(x, shape=[-1, 28, 28, 1])

    # Convolution Layer
    conv1 = conv2d(x, weights['wc1'], biases['bc1'])
    # Max Pooling (down-sampling)
    # conv1 = maxpool2d(conv1, k=2)

    # Convolution Layer
    conv2 = conv2d(conv1, weights['wc2'], biases['bc2'])
    # Max Pooling (down-sampling)
    # conv2 = maxpool2d(conv2, k=2)

    # Fully connected layer
    # Reshape conv2 output to fit fully connected layer input
    fc1 = tf.reshape(conv2, [-1, weights['wd1'].get_shape().as_list()[0]])
    fc1 = tf.add(tf.matmul(fc1, weights['wd1']), biases['bd1'])
    fc1 = tf.nn.relu(fc1)
    # Apply Dropout
    # fc1 = tf.nn.dropout(fc1, dropout)

    # Output, class prediction
    out = tf.add(tf.matmul(fc1, weights['wout']), biases['bout'])
    return out


# Store layers weight & bias
weights = {
    # 5x5 conv, 1 input, 32 outputs
    # Original dimensions: 5, 5, 1, 32
    'wc1': tf.Variable(tf.random_normal([kernel_shape[0], kernel_shape[1], 1, n_kernels[0]])),
    # 5x5 conv, 32 inputs, 64 outputs
    # Original dimensions: 5, 5, 32, 64
    'wc2': tf.Variable(tf.random_normal([kernel_shape[0], kernel_shape[1], n_kernels[0], n_kernels[1]])),
    # fully connected, 7*7*64 inputs, 1024 outputs
    'wd1': tf.Variable(tf.random_normal([kernel_shape[0] * kernel_shape[1] * n_kernels[1], 256])),  # 7*7*64, 1024])),
    # 1024 inputs, 10 outputs (class prediction)
    'wout': tf.Variable(tf.random_normal([256, n_classes]))
}

biases = {
    'bc1': tf.Variable(tf.random_normal([n_kernels[0]])),
    'bc2': tf.Variable(tf.random_normal([n_kernels[1]])),
    'bd1': tf.Variable(tf.random_normal([256])),
    'bout': tf.Variable(tf.random_normal([n_classes]))
}

# Construct model
pred = conv_net(x, weights, biases)  # , keep_prob)

# Define loss and optimizer
cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=pred, labels=y))
optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate).minimize(cost)

# Evaluate model
correct_pred = tf.equal(tf.argmax(pred, 1), tf.argmax(y, 1))
accuracy = tf.reduce_mean(tf.cast(correct_pred, tf.float32))

# Initializing the variables
init = tf.global_variables_initializer()

# Session configuration
config = tf.ConfigProto(device_count={'GPU': 0})
# Launch the graph
with tf.Session(config=config) as sess:
    sess.run(init)
    step = 1
    # Keep training until reach max iterations
    acc = 0
    # while step * batch_size < training_iters:
    while acc < 0.8:
        batch_x, batch_y = mnist.train.next_batch(batch_size)
        # Run optimization op (backprop)
        sess.run(optimizer, feed_dict={x: batch_x, y: batch_y,
                                       keep_prob: dropout})
        if step % display_step == 0:
            # Calculate batch loss and accuracy
            loss, acc = sess.run([cost, accuracy], feed_dict={x: batch_x,
                                                              y: batch_y})  # ,
            # keep_prob: 1.})
            print("Iter " + str(step * batch_size) + ", Minibatch Loss= " +
                  "{:.6f}".format(loss) + ", Training Accuracy= " +
                  "{:.5f}".format(acc))
        step += 1
    print("Optimization Finished!")

    # Calculate accuracy for 256 mnist test images
    print("Testing Accuracy:",
          sess.run(accuracy, feed_dict={x: mnist.test.images[:256],
                                        y: mnist.test.labels[:256],
                                        keep_prob: 1.}))
    # Backup params
    trained_weights = sess.run(weights)
    trained_biases = sess.run(biases)

dir_name = "experiment." + str(n_kernels[0]) + "." + str(n_kernels[1]) + "." + \
           str(kernel_shape[0]) + "." + str(kernel_shape[1])
# Create directory
if not os.path.isdir(dir_name):
    os.mkdir(dir_name)

trained_params = {}
for weight_key in trained_weights:
    if len(trained_weights[weight_key].shape) == 2:
        # MLP weights
        trained_params[weight_key] = trained_weights[weight_key].transpose()
    else:
        # Convolutional weights
        trained_params[weight_key] = trained_weights[weight_key]
trained_params = {**trained_params, **trained_biases}

for param_name in trained_params:
    json_string = json.dumps(trained_params[param_name].astype(np.float16).tolist())
    print(trained_params[param_name].shape)
    with open(dir_name + '/' + param_name + '.json', 'w') as outfile:
        outfile.write(json_string)
        outfile.close()
    print("Saved param \"" + param_name + "\"")

start = 0
end = 1

test_images, test_targets = mnist.test.next_batch(100)

test_batch_images = test_images[start:end]
test_batch_targets = test_targets[start:end]

# Save test images
json_string = json.dumps(test_batch_images.astype(np.float32).tolist())
with open(dir_name + '/test_images_n' + str(end - start) + '.json', 'w') as outfile:
    outfile.write(json_string)
    outfile.close()

print("Saved " + str(test_batch_images.shape[0]) + " images, shape: ", end='')
print(test_batch_images.shape)

# Save Tensorflow's forward propagation results into a JSON file
print("Forward-propagating...")
n_input = 784
x = tf.placeholder("float", [None, n_input])

# Convert arrays to tensors
trained_weights_tensors = {}
for weight_name in trained_weights:
    trained_weights_tensors[weight_name] = tf.convert_to_tensor(
        trained_weights[weight_name].astype(np.float16).astype(np.float32))

trained_biases_tensors = {}
for bias_name in trained_biases:
    trained_biases_tensors[bias_name] = tf.convert_to_tensor(
        trained_biases[bias_name].astype(np.float16).astype(np.float32))

# Construct model
pred = conv_net(x, trained_weights_tensors, trained_biases_tensors)

# Initializing the variables
init = tf.initialize_all_variables()

# Launch the graph
with tf.Session(config=config) as sess:
    # run_options = tf.RunOptions(trace_level=tf.RunOptions.FULL_TRACE)
    # run_metadata = tf.RunMetadata()
    sess.run(init)
    # Produce outputs
    result = sess.run([pred], feed_dict={x: test_batch_images})  # , options=run_options, run_metadata=run_metadata)
    # Create the Timeline object, and write it to a json
    # tl = timeline.Timeline(run_metadata.step_stats)
    # ctf = tl.generate_chrome_trace_format()
    # current_time = datetime.datetime.now()
    # if not os.path.isdir(dir_name + "/results_tensorflow"):
    #    os.mkdir(dir_name + "/results_tensorflow")
    # with open(dir_name + "/results_tensorflow/" + current_time.strftime("%d.%m.%Y-%H.%M.%S.") +
    #          str(int(current_time.microsecond / 1000)).zfill(3) + ".n" + str(end-start) +
    #          ".timeline.json", 'w') as f:
    #    f.write(ctf)

# Print results
verbose = True
if verbose:
    np.set_printoptions(threshold=np.inf, suppress=True)
    # print("Weights[0][0]:")
    # print(trained_weights)
    i = 0
    print("Inputs[" + str(i) + "]:")
    print(test_batch_images[i].sum())
    # print("Weights['wc1'][0][0]:")
    # print(trained_weights['wc1'][0][0])
    # print("Biases['bc1']:")
    # print(trained_biases['bc1'])
    print("Weights['wd1'][0]:")
    print(trained_weights['wd1'][0])
    print("Biases['bd1']:")
    print(trained_biases['bd1'])
    print("Output[" + str(i) + "]:")
    print(result[0][i].shape)
    print(result[0][i])
    print("Output[0:" + str(i + 1) + "] maxed:")
    print([list(decision).index(max(decision)) for decision in result[0][:i + 1]])
    print("Correct[0:" + str(i + 1) + "]:")
    print([list(decision).index(max(decision)) for decision in test_batch_targets[:i + 1]])

# Save results
json_string = json.dumps(result[0].astype(np.float32).tolist())
with open(dir_name + '/test_tf_results_n' + str(end - start) + '.json', 'w') as outfile:
    outfile.write(json_string)
    outfile.close()
if verbose:
    print("Saved results, shape: ", end='')
    print(result[0].shape)
