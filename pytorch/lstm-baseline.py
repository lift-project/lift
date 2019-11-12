"""
Based on the code by Jessica Yung(1) and Gabriel Loye(2)
Extended by Naums Mogers
1. https://www.jessicayung.com/lstms-for-time-series-in-pytorch/
2. https://blog.floydhub.com/long-short-term-memory-from-zero-to-hero-with-pytorch/
"""
import torch
import torch.nn as nn
from generate_data import *
import matplotlib.pyplot as plt
import argparse
from os.path import join
import pickle
from torch.utils.data import TensorDataset, DataLoader
import javaobj


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
# noise_var = 0
noise_var = 0.05
num_datapoints = 8192
batch_size = 256

num_test = 256
num_train = num_datapoints - num_test
test_size = num_test / num_datapoints

# Network params
input_time_sequence_size = 20
input_vector_length = 32
# If `per_element` is True, then LSTM reads in one timestep at a time.
per_element = True
if per_element:
    lstm_input_sequence_size = 1
else:
    lstm_input_sequence_size = input_time_sequence_size
# size of hidden layers
h1 = 64
output_dim = 1
num_layers = 2
learning_rate = 0.005
# learning_rate = 1e-1
num_epochs = 5
dtype = torch.float

weights_file_path = join(args.train_out_dir, "trained_lstm_weights.serial")
inputs_file_path = join(args.train_out_dir, "lstm_inputs.pickle")
gold_outputs_file_path = join(args.train_out_dir, "lstm_gold_outputs.pickle")
pytorch_outputs_file_path = join(args.train_out_dir, "pytorch_gold_outputs.pickle")
pytorch_lstm0_out_file_path = join(args.train_out_dir, "pytorch_lstm0_outputs.pickle")
pytorch_lstm_pre_test_h_state_file_path = join(args.train_out_dir, "pytorch_lstm_pre_test_h_state.pickle")
pytorch_lstm_post_test_h_state_file_path = join(args.train_out_dir, "pytorch_lstm_post_test_h_state.pickle")

#####################
# Build model
#####################

# Here we define our model as a class
class LSTM(nn.Module):

    def __init__(self, input_seq_size, hidden_dim, batch_size, output_dim, num_layers, vector_len):
        super(LSTM, self).__init__()
        self.input_seq_size = input_seq_size
        self.hidden_dim = hidden_dim
        self.set_batch_size(batch_size)
        self.num_layers = num_layers
        self.vector_len = vector_len

        torch.random.manual_seed(8)
        np.random.seed(8)

        # Define the LSTM layer
        self.lstm = nn.LSTM(input_size=self.vector_len, hidden_size=self.hidden_dim, 
                            num_layers=self.num_layers, batch_first=True)

        # Define the output layers
        self.linear = nn.Linear(self.hidden_dim, output_dim)
        # self.sigmoid = nn.ReLU()

    def set_batch_size(self, batch_size):
        self.batch_size = batch_size

    def init_hidden_state(self):
        shape = (self.num_layers, self.batch_size, self.hidden_dim)
        # This is what we'll initialise our hidden state as
        # return (torch.zeros(shape),
        #         torch.zeros(shape))
        return (torch.randn(shape),
                torch.randn(shape))
        # weight = next(self.parameters()).data
        # hidden = (weight.new(shape).zero_(),
        #               weight.new(shape).zero_())
        # return hidden

    def forward(self, input, hidden_state):
        # Forward pass through LSTM layer
        # shape of lstm_out: [input_size, batch_size, hidden_dim]
        # shape of self.hidden: (a, b), where a and b both 
        # have shape (num_layers, batch_size, hidden_dim).
        lstm_out, hidden_state = self.lstm(input, hidden_state)
        
        # Only take the output from the final timestep
        # Can pass on the entirety of lstm_out to the next layer if it is a seq2seq prediction
        y_pred = self.linear(lstm_out[:, -1, :])
        return lstm_out, y_pred.view(-1), hidden_state


def init_model():
    return (
        LSTM(input_seq_size=lstm_input_sequence_size, hidden_dim=h1, batch_size=batch_size, #batch_size=num_train, 
            output_dim=output_dim, 
            num_layers=num_layers, vector_len=input_vector_length),
        torch.nn.MSELoss())
        # torch.nn.MSELoss(size_average=False))


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
    print(X_train.shape)
    print(y_train.shape)


    train_data = TensorDataset(X_train, y_train)
    test_data = TensorDataset(X_test, y_test)

    train_loader = DataLoader(train_data, shuffle=True, batch_size=batch_size)
    test_loader = DataLoader(test_data, shuffle=True, batch_size=batch_size)

    #####################
    # Train model
    #####################

    (model, loss_fn) = init_model()

    optimiser = torch.optim.Adam(model.parameters(), lr=learning_rate)

    hist = [] #np.zeros(num_epochs)

    model.train()

    for t in range(num_epochs):
        # Initialise hidden state
        # Don't do this if you want your LSTM to be stateful
        hidden_state = model.init_hidden_state()

        all_y_pred = []
        i = 0
        for inputs, labels in train_loader:

            model.zero_grad()
        
            # Forward pass
            _, y_pred, hidden_state = model(inputs, hidden_state)

            hidden_state[0].detach_()
            hidden_state[1].detach_()

            loss = loss_fn(y_pred, labels)
            if i % 5 == 0:
                print("Epoch ", t, "Batch ", i, "MSE: ", loss.item())
            hist += [loss.item()]

            # Backward pass
            loss.backward() #retain_graph=True)

            nn.utils.clip_grad_norm_(model.parameters(), 5)

            # Update parameters
            optimiser.step()
            i += 1
            
            all_y_pred = all_y_pred + y_pred.tolist()

    #####################
    # Test
    #####################
    hidden_state = model.init_hidden_state()
    pre_test_hidden_state = (hidden_state[0].numpy().copy(), hidden_state[1].numpy().copy())

    lstm0_out, y_test_pred, post_test_hidden_state = \
        test(model, loss_fn, X_test, y_test, pre_test_hidden_state)

    #####################
    # Save data to disk
    #####################
    backup_model_and_data(model, X_test, y_test, y_test_pred, lstm0_out, 
        pre_test_hidden_state, post_test_hidden_state)

    #####################
    # Plot preds and performance
    #####################

    plt.plot(y_train.detach().numpy(), label="Train targets")
    plt.plot(all_y_pred, label="Train preds")
    plt.legend()
    plt.show()

    plt.plot(hist, label="Training loss")
    plt.legend()
    plt.show()


def test(model, loss_fn, X_test, y_test, pre_test_hidden_state,
         y_test_pred_post_train = None, 
         lstm0_out_post_train = None):

    #####################
    # Test
    #####################
    model.eval()
    hidden_state_tensor = (torch.from_numpy(pre_test_hidden_state[0]), torch.from_numpy(pre_test_hidden_state[1]))

    model.set_batch_size(num_test)

    lstm0_out, y_test_pred, _ = model(X_test, hidden_state_tensor)

    post_test_hidden_state = (hidden_state_tensor[0].numpy().copy(), hidden_state_tensor[1].numpy().copy())

    # backup_model_and_data(model, X_test, y_test, y_test_pred, lstm0_out)

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

    plt.plot(y_test_pred.detach().numpy(), label="Test preds", linewidth=7.0, marker='o')
    plt.plot(y_test.detach().numpy(), label="Test targets", marker='o')
    if y_test_pred_post_train is not None:  
        plt.plot(y_test_pred_post_train.detach().numpy(), label="Post-train test preds", color="black", marker='o')
    plt.legend()
    plt.show()

    return lstm0_out, y_test_pred, post_test_hidden_state


def save_tensor_to_csv(tensor: torch.Tensor, filepath: str):
    print(tensor.shape)
    cols = tensor.shape[-1]
    assert(cols >= len(tensor.shape))
    header = ",".join(list(map(lambda i: str(i), list(tensor.shape))) + [""] * (cols - len(tensor.shape)))
    np.savetxt(filepath, tensor.reshape([-1, cols]).numpy(), delimiter=",",header=header, comments='')

def save_ndarray_to_csv(ndarray: np.ndarray, filepath: str):
    print(ndarray.shape)
    cols = ndarray.shape[-1]
    assert(cols >= len(ndarray.shape))
    header = ",".join(list(map(lambda i: str(i), list(ndarray.shape))) + [""] * (cols - len(ndarray.shape)))
    np.savetxt(filepath, ndarray.reshape([-1, cols]), delimiter=",",header=header, comments='')


def backup_model_and_data(model, X_test, y_test, y_test_pred, lstm0_out, pre_test_hidden_state, post_test_hidden_state):
    #####################
    # Save data to disk
    #####################
    torch.save(model.state_dict(), weights_file_path)
    pickle.dump(X_test, open(inputs_file_path, "wb"))
    pickle.dump(y_test, open(gold_outputs_file_path, "wb"))    
    pickle.dump(y_test_pred, open(pytorch_outputs_file_path, "wb")) 
    pickle.dump(lstm0_out, open(pytorch_lstm0_out_file_path, "wb")) 
    pickle.dump(pre_test_hidden_state, open(pytorch_lstm_pre_test_h_state_file_path, "wb"))
    pickle.dump(post_test_hidden_state, open(pytorch_lstm_post_test_h_state_file_path, "wb"))

    # print(model.state_dict().keys())

    save_tensor_to_csv(tensor=X_test, 
        filepath=join(args.train_out_dir, "lstm_inputs.csv"))

    save_ndarray_to_csv(ndarray=pre_test_hidden_state[0], 
        filepath=join(args.train_out_dir, "lstm_pre_test_hidden_state_l0.csv"))

    save_tensor_to_csv(tensor=model.state_dict()["lstm.weight_ih_l0"], 
        filepath=join(args.train_out_dir, "lstm.weight_ih_l0.csv"))

    save_tensor_to_csv(tensor=model.state_dict()["lstm.bias_ih_l0"], 
        filepath=join(args.train_out_dir, "lstm.bias_ih_l0.csv"))

    save_tensor_to_csv(tensor=model.state_dict()["lstm.weight_hh_l0"], 
        filepath=join(args.train_out_dir, "lstm.weight_hh_l0.csv"))

    save_tensor_to_csv(tensor=model.state_dict()["lstm.bias_hh_l0"], 
        filepath=join(args.train_out_dir, "lstm.bias_hh_l0.csv"))

    save_tensor_to_csv(tensor=lstm0_out.detach(), 
        filepath=join(args.train_out_dir, "lstm0_out.csv"))

    save_ndarray_to_csv(ndarray=post_test_hidden_state[0], 
        filepath=join(args.train_out_dir, "lstm_post_test_hidden_state_l0.csv"))


def restore_model_and_data():
    print("Restoring model and data from disk")

    (model, loss_fn) = init_model()

    model.load_state_dict(torch.load(weights_file_path))
    model.eval()

    X_test = pickle.load(open(inputs_file_path, "rb"))
    y_test = pickle.load(open(gold_outputs_file_path, "rb"))
    y_test_pred_post_train = pickle.load(open(pytorch_outputs_file_path, "rb"))  
    lstm0_out = pickle.load(open(pytorch_lstm0_out_file_path, "rb"))
    pre_test_hidden_state = pickle.load(open(pytorch_lstm_pre_test_h_state_file_path, "rb"))  

    return (model, loss_fn, X_test, y_test, pre_test_hidden_state, y_test_pred_post_train, lstm0_out)

    

def test_without_training():
    test(*restore_model_and_data())



if args.train:
    print("Training the model")
    train()

if args.test:
    print("Testing the model")
    test_without_training()