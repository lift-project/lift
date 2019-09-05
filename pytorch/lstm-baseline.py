"""
Based on the code by Jessica Yung 
Extended by Naums Mogers
https://www.jessicayung.com/lstms-for-time-series-in-pytorch/
"""
import torch
import torch.nn as nn
from generate_data import *
import matplotlib.pyplot as plt
import argparse
from os.path import join
import pickle


parser = argparse.ArgumentParser()
parser.add_argument("--train_out_dir", help="Directory for storing trained weights and generated inputs/outputs",
                    type=str, required=True)
action = parser.add_mutually_exclusive_group(required=True)
action.add_argument('--train', action='store_true', help='Train the model and save weights, inputs and outputs to disk')
action.add_argument('--test', action='store_true', 
    help='Test the model by restoring weights from disk, producing new outputs and comparing with those in disk')

args = parser.parse_args()


#####################
# Set parameters
#####################

# Data params
noise_var = 0
num_datapoints = 100
test_size = 0.2
num_train = int((1-test_size) * num_datapoints)
num_test = num_datapoints - num_train

# Network params
input_time_sequence_size = 20
input_vector_length = 50
# If `per_element` is True, then LSTM reads in one timestep at a time.
per_element = True
if per_element:
    lstm_input_sequence_size = 1
else:
    lstm_input_sequence_size = input_time_sequence_size
# size of hidden layers
h1 = 32
output_dim = 1
num_layers = 2
learning_rate = 1e-3
num_epochs = 500
dtype = torch.float

weights_file_path = join(args.train_out_dir, "trained_lstm_weights.serial")
inputs_file_path = join(args.train_out_dir, "lstm_inputs.pickle")
gold_outputs_file_path = join(args.train_out_dir, "lstm_gold_outputs.pickle")
pytorch_outputs_file_path = join(args.train_out_dir, "pytorch_gold_outputs.pickle")
pytorch_lstm0_out_file_path = join(args.train_out_dir, "pytorch_lstm0_outputs.pickle")

#####################
# Build model
#####################

# Here we define our model as a class
class LSTM(nn.Module):

    def __init__(self, input_seq_size, hidden_dim, batch_size, output_dim=1,
                    num_layers=2, vector_len=8):
        super(LSTM, self).__init__()
        self.input_seq_size = input_seq_size
        self.hidden_dim = hidden_dim
        self.set_batch_size(batch_size)
        self.num_layers = num_layers
        self.vector_len = vector_len

        # Define the LSTM layer
        self.lstm = nn.LSTM(self.vector_len, self.hidden_dim, self.num_layers)

        # Define the output layer
        self.linear = nn.Linear(self.hidden_dim, output_dim)

    def set_batch_size(self, batch_size):
        self.batch_size = batch_size

    def init_hidden_state(self):
        # This is what we'll initialise our hidden state as
        return (torch.zeros(self.num_layers, self.batch_size, self.hidden_dim),
                torch.zeros(self.num_layers, self.batch_size, self.hidden_dim))

    def forward(self, input):
        # Forward pass through LSTM layer
        # shape of lstm_out: [input_size, batch_size, hidden_dim]
        # shape of self.hidden: (a, b), where a and b both 
        # have shape (num_layers, batch_size, hidden_dim).
        lstm_out, self.hidden = self.lstm(input)
        
        # Only take the output from the final timetep
        # Can pass on the entirety of lstm_out to the next layer if it is a seq2seq prediction
        y_pred = self.linear(lstm_out[-1].view(self.batch_size, -1))
        return lstm_out, y_pred.view(-1)


def init_model():
    return (
        LSTM(lstm_input_sequence_size, h1, batch_size=num_train, output_dim=output_dim, 
            num_layers=num_layers, vector_len=input_vector_length),
        torch.nn.MSELoss(size_average=False))


def train():
    #####################
    # Generate data
    #####################
    data = ARData(num_datapoints, num_prev=input_time_sequence_size, test_size=test_size, noise_var=noise_var,
        vector_len=input_vector_length,
        coeffs=fixed_ar_coefficients[input_time_sequence_size])

    # make training and test sets in torch
    X_train = torch.from_numpy(data.X_train).type(torch.Tensor)
    X_test = torch.from_numpy(data.X_test).type(torch.Tensor)
    y_train = torch.from_numpy(data.y_train).type(torch.Tensor).view(-1)
    y_test = torch.from_numpy(data.y_test).type(torch.Tensor).view(-1)

    X_train = X_train.view([input_time_sequence_size, -1, input_vector_length])
    X_test = X_test.view([input_time_sequence_size, -1, input_vector_length])

    #####################
    # Train model
    #####################

    (model, loss_fn) = init_model()

    optimiser = torch.optim.Adam(model.parameters(), lr=learning_rate)

    hist = np.zeros(num_epochs)

    for t in range(num_epochs):
        # Initialise hidden state
        # Don't do this if you want your LSTM to be stateful
        # model.hidden_state = model.init_hidden_state()
        
        # Forward pass
        _, y_pred = model(X_train)

        loss = loss_fn(y_pred, y_train)
        if t % 100 == 0:
            print("Epoch ", t, "MSE: ", loss.item())
        hist[t] = loss.item()

        # Zero out gradient, else they will accumulate between epochs
        optimiser.zero_grad()

        # Backward pass
        loss.backward()

        # Update parameters
        optimiser.step()

    #####################
    # Test
    #####################
    lstm0_out, y_test_pred = test(model, loss_fn, X_test, y_test)

    #####################
    # Save data to disk
    #####################
    backup_model_and_data(model, X_test, y_test, y_test_pred, lstm0_out)

    #####################
    # Plot preds and performance
    #####################

    plt.plot(y_pred.detach().numpy(), label="Train preds")
    plt.plot(y_train.detach().numpy(), label="Train targets")
    plt.legend()
    plt.show()

    plt.plot(hist, label="Training loss")
    plt.legend()
    plt.show()


def test(model, loss_fn, X_test, y_test, y_test_pred_post_train = None, 
         lstm0_out_post_train = None):
    #####################
    # Test
    #####################
    model.set_batch_size(num_test)

    lstm0_out, y_test_pred = model(X_test)
    test_loss = loss_fn(y_test_pred, y_test)
    if y_test_pred_post_train is not None:
        test_loss_against_post_train_test = loss_fn(y_test_pred, y_test_pred_post_train)
        test_lstm0_loss_against_post_train_test = loss_fn(lstm0_out, lstm0_out_post_train)

    print("Test MSE against golden outputs: ", test_loss.item())
    if y_test_pred_post_train is not None:  
        print("Test MSE against post-train PyTorch outputs: ", 
            test_loss_against_post_train_test.item())
        print("Test LSTM0 output MSE against post-train PyTorch LSTM0 outputs: ", 
            test_lstm0_loss_against_post_train_test.item())
    print("Test targets: ", y_test)
    print("Test outputs: ", y_test_pred)

    plt.plot(y_test_pred.detach().numpy(), label="Test preds", linewidth=7.0)
    plt.plot(y_test.detach().numpy(), label="Test targets")
    if y_test_pred_post_train is not None:  
        plt.plot(y_test_pred_post_train.detach().numpy(), label="Post-train test preds", color="black")
    plt.legend()
    plt.show()

    return lstm0_out, y_test_pred


def backup_model_and_data(model, X_test, y_test, y_test_pred, lstm0_out):
    #####################
    # Save data to disk
    #####################
    torch.save(model.state_dict(), weights_file_path)
    pickle.dump(X_test, open(inputs_file_path, "wb"))
    pickle.dump(y_test, open(gold_outputs_file_path, "wb"))    
    pickle.dump(y_test_pred, open(pytorch_outputs_file_path, "wb")) 
    pickle.dump(lstm0_out, open(pytorch_lstm0_out_file_path, "wb")) 


def restore_model_and_data():
    print("Restoring model and data from disk")

    (model, loss_fn) = init_model()

    model.load_state_dict(torch.load(weights_file_path))
    model.eval()

    X_test = pickle.load(open(inputs_file_path, "rb"))
    y_test = pickle.load(open(gold_outputs_file_path, "rb"))            
    y_test_pred_post_train = pickle.load(open(pytorch_outputs_file_path, "rb"))  
    lstm0_out = pickle.load(open(pytorch_lstm0_out_file_path, "rb"))  

    return (model, loss_fn, X_test, y_test, y_test_pred_post_train, lstm0_out)

    

def test_without_training():
    test(*restore_model_and_data())



if args.train:
    print("Training the model")
    train()

if args.test:
    print("Testing the model")
    test_without_training()