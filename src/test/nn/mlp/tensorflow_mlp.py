'''
A Multilayer Perceptron implementation example using TensorFlow library.
This example is using the MNIST database of handwritten digits
(http://yann.lecun.com/exdb/mnist/)

Author: Aymeric Damien
Project: https://github.com/aymericdamien/TensorFlow-Examples/

Changed by Naums Mogers
'''
import tensorflow as tf
import pickle
import numpy as np
import json
import datetime
from tensorflow.python.client import timeline
import os

verbose = True

# Import MINST data
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("/tmp/data/", one_hot=True)

test_images, test_targets = mnist.test.next_batch(10000)
# Shuffle manually, as next_batch() returns the same result after initialization
perm = np.arange(10000)
np.random.shuffle(perm)
#test_images = test_images[perm]
#test_targets = test_targets[perm]
test_batch_no = 0

# Parameters
learning_rate = 0.001
training_epochs = 15
batch_size = 100
display_step = 1

# Network Parameters
#n_hidden_1 = 256 # 1st layer number of features
#n_hidden_2 = 256 # 2nd layer number of features
n_input = 784 # MNIST data input (img shape: 28*28)
n_classes = 10 # MNIST total classes (0-9 digits)


def SimpleEncode(ndarray):
    return json.dumps(ndarray.tolist())

# Create model
def multilayer_perceptron(x, weights, biases, funcs):
    input_data = x
    i = 0
    #for (weights_layer, biases_layer, func_layer) in zip(weights[0:2], biases[0:2], funcs[0:2]):
    for (weights_layer, biases_layer, func_layer) in zip(weights, biases, funcs):
        layer_output = tf.add(tf.matmul(input_data, weights_layer), biases_layer)
        if func_layer is not None:
            layer_output = func_layer(layer_output)
        input_data = layer_output
        #if i == 0:
        #    break
        i = i + 1
    # Hidden layer with RELU activation
    #layer_1 = tf.add(tf.matmul(x, weights['h1']), biases['b1'])
    #layer_1 = tf.nn.relu(layer_1)
    # Hidden layer with RELU activation
    #layer_2 = tf.add(tf.matmul(layer_1, weights['h2']), biases['b2'])
    #layer_2 = tf.nn.relu(layer_2)
    # Output layer with linear activation
    #out_layer = tf.matmul(layer_2, weights['out']) + biases['out']
    return layer_output

def create_exp_dir_name(hidden_layers):
    #dir_name = "experiment." + str(n_input)
    #for n_hidden in hidden_layers:
    #    dir_name = dir_name + "-" + str(n_hidden)
    #dir_name = dir_name + "-" + str(n_classes)
    dir_name = "experiment." + str(hidden_layers[0])

    # Create directory
    if not os.path.isdir(dir_name):
        os.mkdir(dir_name)

    return dir_name


def train_and_forward_propagate(hidden_layers, inputs_tofeed):
    dir_name = create_exp_dir_name(hidden_layers)
    # tf Graph input
    x = tf.placeholder("float", [None, n_input])
    y = tf.placeholder("float", [None, n_classes])

    # Store layers weight & bias
    weights = []
    biases = []
    funcs = []
    previous_layer_n = n_input
    for n_hidden in hidden_layers:
        weights.append(tf.Variable(tf.random_normal([previous_layer_n, n_hidden])))
        biases.append(tf.Variable(tf.random_normal([n_hidden])))
        funcs.append(tf.nn.relu)
        previous_layer_n = n_hidden

    weights.append(tf.Variable(tf.random_normal([previous_layer_n, n_classes])))
    biases.append(tf.Variable(tf.random_normal([n_classes])))
    funcs.append(None)
    #weights = {
    #    'h1': tf.Variable(tf.random_normal([n_input, n_hidden_1])),
    #    'h2': tf.Variable(tf.random_normal([n_hidden_1, n_hidden_2])),
    #    'out': tf.Variable(tf.random_normal([n_hidden_2, n_classes]))
    #}
    #biases = {
    #    'b1': tf.Variable(tf.random_normal([n_hidden_1])),
    #    'b2': tf.Variable(tf.random_normal([n_hidden_2])),
    #    'out': tf.Variable(tf.random_normal([n_classes]))
    #}

    # Construct model
    pred = multilayer_perceptron(x, weights, biases, funcs)

    # Define loss and optimizer
    cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(pred, y))
    optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate).minimize(cost)

    # Initializing the variables
    init = tf.initialize_all_variables()

        # Launch the graph
    with tf.Session() as sess:
        sess.run(init)

        # Training cycle
        for epoch in range(training_epochs):
            avg_cost = 0.
            total_batch = int(mnist.train.num_examples/batch_size)
            # Loop over all batches
            for i in range(total_batch):
                batch_x, batch_y = mnist.train.next_batch(batch_size)
                # Run optimization op (backprop) and cost op (to get loss value)
                _, c = sess.run([optimizer, cost], feed_dict={x: batch_x,
                                                              y: batch_y})
                # Compute average loss
                avg_cost += c / total_batch
            # Display logs per epoch step
            if epoch % display_step == 0:
                print("Epoch:", '%04d' % (epoch+1), "cost=", \
                    "{:.9f}".format(avg_cost))

        print("Optimization Finished!")

        # Test model
        correct_prediction = tf.equal(tf.argmax(pred, 1), tf.argmax(y, 1))
        # Calculate accuracy
        accuracy = tf.reduce_mean(tf.cast(correct_prediction, "float"))
        print("Accuracy:", accuracy.eval({x: mnist.test.images, y: mnist.test.labels}))

        # Backup params
        trained_weights = sess.run(weights)
        trained_biases = sess.run(biases)

    (start, end) = get_start_end(inputs_tofeed, test_batch_no)

    pickle.dump((trained_weights, trained_biases, funcs),
                open(dir_name + "/pickled_params.p", "wb"))
                #open(dir_name + "/pickled_params_n" + str(end-start) +".p", "wb"))

    param_names = []
    params = []
    for i in range(0, len(weights) - 1):
        param_names.append("W" + str(i+1))
        param_names.append("b" + str(i+1))
        params.append(trained_weights[i].transpose())
        params.append(trained_biases[i].transpose())
    param_names.append("Wout")
    param_names.append("bout")
    params.append(trained_weights[-1].transpose())
    params.append(trained_biases[-1].transpose())
    #param_names = ["W1", "b1", "W2", "b2", "Wout", "bout"]
    #params = [trained_weights['h1'].transpose(), trained_biases['b1'],
    #          trained_weights['h2'].transpose(), trained_biases['b2'],
    #          trained_weights['out'].transpose(), trained_biases['out']]

    i = 0
    for param_name in param_names:
        json_string = SimpleEncode(params[i].astype(np.float32))
        print(params[i].shape)
        #%timeit SimpleDecode(json_string)
        with open(dir_name + '/' + param_name + '_n' + str(end-start) + '.json', 'w') as outfile:
            outfile.write(json_string)
            outfile.close()
        print(str(i) + ". Saved param \"" + param_name + "\", shape: ", end='')
        #print(params[i][0])
        i = i + 1

    # Generate appriate outputs
    forward_propagate(hidden_layers, inputs_tofeed)

def get_start_end(inputs_tofeed, test_batch_no):
    start = inputs_tofeed
    end = start + inputs_tofeed

    return (start, end)


def forward_propagate(hidden_layers, inputs_tofeed):
    global test_batch_no

    dir_name = create_exp_dir_name(hidden_layers)

    (start, end) = get_start_end(inputs_tofeed, test_batch_no)
    test_batch_images = test_images[start:end]
    test_batch_targets = test_targets[start:end]
    print(start, end, inputs_tofeed)

    ### Save parameters, inputs, outputs and targets into JSON files
    trained_weights, trained_biases, funcs = \
         pickle.load(open(dir_name + "/pickled_params.p", "rb"))
    #    pickle.load(open(dir_name + "/pickled_params_n" + str(end-start) +".p", "rb"))

    # Save test images
    json_string = SimpleEncode(test_batch_images.astype(np.float32))
    with open(dir_name + '/test_images_n' + str(end-start) +'.json', 'w') as outfile:
        outfile.write(json_string)
        outfile.close()
    print("Saved " + str(test_batch_images.shape[0]) + " images, shape: ", end='')
    print(test_batch_images.shape)

    ### Save Tensorflow's forward propagation results into a JSON file
    print("Forward-propagating...")
    x = tf.placeholder("float", [None, n_input])

    # Construct model
    pred = multilayer_perceptron(x, trained_weights, trained_biases, funcs)
    init = tf.initialize_all_variables()

    # Launch the graph
    with tf.Session() as sess:
        run_options = tf.RunOptions(trace_level=tf.RunOptions.FULL_TRACE)
        run_metadata = tf.RunMetadata()
        sess.run(init)
        # Produce outputs
        result = sess.run([pred], feed_dict={x: test_batch_images}, options=run_options, run_metadata=run_metadata)
        # Create the Timeline object, and write it to a json
        tl = timeline.Timeline(run_metadata.step_stats)
        ctf = tl.generate_chrome_trace_format()
        current_time = datetime.datetime.now()
        if not os.path.isdir(dir_name + "/results_tensorflow"):
            os.mkdir(dir_name + "/results_tensorflow")
        with open(dir_name + "/results_tensorflow/" + current_time.strftime("%d.%m.%Y-%H.%M.%S.") +
                  str(int(current_time.microsecond / 1000)).zfill(3) + ".n" + str(end-start) +
                  ".timeline.json", 'w') as f:
            f.write(ctf)

    # Print results
    if verbose:
        np.set_printoptions(threshold=np.inf, suppress=True)
        print("Weights[1][0]:")
        print(trained_weights[1].transpose()[0])
        print("Biases[1][0]:")
        print(trained_biases[1].transpose()[0])
        i = 22
        print("Inputs[" + str(i) + "]:")
        print(test_batch_images[i])
        print("Output[0:" + str(i) + "]:")
        print(result[0][0:i])
        print("Output[0:" + str(i+1) + "] maxed:")
        print([list(decision).index(max(decision)) for decision in result[0][:i+1]])
        print("Correct[0:" + str(i+1) + "]:")
        print([list(decision).index(max(decision)) for decision in test_batch_targets[:i+1]])
        print(len(trained_weights[1]))
    # Save results
    json_string = SimpleEncode(result[0].astype(np.float32))
    with open(dir_name + '/test_tf_results_n' + str(end-start) + '.json', 'w') as outfile:
        outfile.write(json_string)
        outfile.close
    if verbose:
        print("Saved results, shape: ", end='')
        print(result[0].shape)

    test_batch_no = test_batch_no + 1
