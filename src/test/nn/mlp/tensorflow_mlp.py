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

# Import MINST data
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("/tmp/data/", one_hot=True)

test_images, test_targets = mnist.test.next_batch(10000)
# Shuffle manually, as next_batch() returns the same result after initialization
perm = np.arange(10000)
np.random.shuffle(perm)
test_images = test_images[perm]
test_targets = test_targets[perm]
test_batch_no = 0

# Parameters
learning_rate = 0.001
training_epochs = 15
batch_size = 100
display_step = 1

# Network Parameters
n_hidden_1 = 256 # 1st layer number of features
n_hidden_2 = 256 # 2nd layer number of features
n_input = 784 # MNIST data input (img shape: 28*28)
n_classes = 10 # MNIST total classes (0-9 digits)

# tf Graph input
x = tf.placeholder("float", [None, n_input])
y = tf.placeholder("float", [None, n_classes])


def SimpleEncode(ndarray):
    return json.dumps(ndarray.tolist())

# Create model
def multilayer_perceptron(x, weights, biases):
    # Hidden layer with RELU activation
    layer_1 = tf.add(tf.matmul(x, weights['h1']), biases['b1'])
    layer_1 = tf.nn.relu(layer_1)
    # Hidden layer with RELU activation
    layer_2 = tf.add(tf.matmul(layer_1, weights['h2']), biases['b2'])
    layer_2 = tf.nn.relu(layer_2)
    # Output layer with linear activation
    out_layer = tf.matmul(layer_2, weights['out']) + biases['out']
    return out_layer

def train():
    # Store layers weight & bias
    weights = {
        'h1': tf.Variable(tf.random_normal([n_input, n_hidden_1])),
        'h2': tf.Variable(tf.random_normal([n_hidden_1, n_hidden_2])),
        'out': tf.Variable(tf.random_normal([n_hidden_2, n_classes]))
    }
    biases = {
        'b1': tf.Variable(tf.random_normal([n_hidden_1])),
        'b2': tf.Variable(tf.random_normal([n_hidden_2])),
        'out': tf.Variable(tf.random_normal([n_classes]))
    }

    # Construct model
    pred = multilayer_perceptron(x, weights, biases)

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
        
    pickle.dump((trained_weights, trained_biases), open("pickled_params.p", "wb"))

    param_names = ["W1", "b1", "W2", "b2", "Wout", "bout"]
    params = [trained_weights['h1'].transpose(), trained_biases['b1'],
              trained_weights['h2'].transpose(), trained_biases['b2'],
              trained_weights['out'].transpose(), trained_biases['out']]

    i = 0
    for param_name in param_names:
        print(str(i) + ". Saved param \"" + param_name + "\", shape: ", end='')
        json_string = SimpleEncode(params[i].astype(np.float32))
        print(params[i].shape)
        #%timeit SimpleDecode(json_string)
        with open(param_name + '.json', 'w') as outfile:
            outfile.write(json_string)
            outfile.close
        #print(params[i][0])
        i = i + 1
        
def forward_propagate():
    global test_batch_no
    ### Save parameters, inputs, outputs and targets into JSON files
    trained_weights, trained_biases = pickle.load(open("pickled_params.p", "rb"))
    
    inputs_tofeed = 328
    start = test_batch_no * inputs_tofeed
    end = min(10000, start + inputs_tofeed)
    test_batch_images = test_images[start:end]
    test_batch_targets = test_targets[start:end]

    # Save test images
    json_string = SimpleEncode(test_batch_images.astype(np.float32))
    with open('test_images.json', 'w') as outfile:
        outfile.write(json_string)
        outfile.close
    print("Saved " + str(test_batch_images.shape[0]) + " images, shape: ", end='')
    print(test_batch_images.shape)

    ### Save Tensorflow's forward propagation results into a JSON file
    print("Forward-propagating...")
    x = tf.placeholder("float", [None, n_input])

    # Construct model
    pred = multilayer_perceptron(x, trained_weights, trained_biases)
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
        with open("results_tensorflow/" + current_time.strftime("%d.%m.%Y-%H.%M.%S.") + 
                  str(int(current_time.microsecond / 1000)).zfill(3) + 
                  ".timeline.json", 'w') as f:
            f.write(ctf)
        
    # Print results
    print("Output[:2]:")
    print(result[0][:2])
    print("Output[:2] maxed:")
    print([list(decision).index(max(decision)) for decision in result[0][:2]])
    print("Correct[:2]:")
    print([list(decision).index(max(decision)) for decision in test_batch_targets[0:2]])
        
    # Save results
    json_string = SimpleEncode(result[0].astype(np.float32))
    with open('test_tf_results.json', 'w') as outfile:
        outfile.write(json_string)
        outfile.close
    print("Saved results, shape: ", end='')
    print(result[0].shape)

    test_batch_no = test_batch_no + 1