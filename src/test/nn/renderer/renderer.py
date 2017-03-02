import numpy as np
from collections import namedtuple
import os
import pandas as pd

""" Types """
Directory = namedtuple("Directory", ["path", "name"])

""" Renderer options """
experiment_configs = [
    {"device_name": ["GeForce GTX TITAN Black"], "n_neurons_l0_nonpadded": [288], "n_inputs_l0_nonpadded": [512],
     "mults_per_thread": np.arange(1, 16), "neurons_per_wrg": np.arange(1, 16)}]

imgPath = os.path.abspath("./img")
if not os.path.exists(imgPath):
    os.mkdir(imgPath)

""" Load results """


def load_experiments():
    """
    Loads Lift results.
    :return: Experiment results
    """
    expDirAbs = os.path.abspath("../mlp/")  # Experiments directory
    results_lift = None
    i = 0
    dirs = [Directory(path=expDirAbs, name=d) for d in filter(
        lambda d: os.path.isdir(os.path.join(expDirAbs, d)) and d.startswith("experiment.") and
                  os.path.isdir(os.path.join(expDirAbs, d, "results_lift")),
        os.listdir(expDirAbs))]

    for eDir in dirs:
        resultsLiftDir = os.path.join(eDir.path, eDir.name, "results_lift")
        print(eDir)

        for filename in os.listdir(resultsLiftDir):
            single_csv = pd.read_csv(os.path.join(resultsLiftDir, filename), index_col=False)
            single_csv["datetime"] = filename[:filename.find("_")]
            if results_lift is None:
                results_lift = single_csv
            else:
                results_lift = results_lift.append(single_csv, ignore_index=True)
            i += 1
            if i % 20 == 0:
                print(".", end="")
        print()
    return results_lift


nonfiltered_results = load_experiments()
if nonfiltered_results is None:
    print("No results found!")
    quit()

print("%d non-filtered results." % len(nonfiltered_results))

""" Filter results """
results = [nonfiltered_results[np.all([key in nonfiltered_results and
                                       nonfiltered_results[key].isin(values)
                                       for key, values in config.items()], axis=0)]
           for config in experiment_configs]

print("Loaded " + str(len(results)) + " experiment(s):")
for exp_i in range(len(results)):
    print("Experiment " + str(exp_i) + ": " + str(len(results[exp_i])) + " records")

""" Format data """
# Runtimes: s -> ms
original_status = pd.options.mode.chained_assignment
pd.options.mode.chained_assignment = None
for result in results:
    result.loc[:, "runtime_l0"] = result["runtime_l0"].apply(lambda x: x * 1000)
    result.loc[:, "runtime_l1"] = result["runtime_l1"].apply(lambda x: x * 1000)
    result.loc[:, "runtime_l2"] = result["runtime_l2"].apply(lambda x: x * 1000)
pd.options.mode.chained_assignment = original_status
print("Data was formatted.")
data_is_reset = False


def performance(df):
    """

    :param df:
    :return:
    """
    return (df["n_inputs_l0_nonpadded"] * df["n_neurons_l0_nonpadded"]) / df["runtime_l0"] if not df.empty else 0


def standardize(matrices):
    """
    Scale values to the range from 0 to 1
    :param matrices:
    :return:
    """
    global_max = 0
    global_min = 999

    for i in range(len(matrices)):
        if matrices[i].max() > global_max:
            global_max = matrices[i].max()
        if matrices[i].min() < global_min:
            global_min = matrices[i].min()
    for i in range(len(matrices)):
        matrices[i] = (matrices[i] - global_min) / (global_max - global_min)
    return matrices


""" Prepare data """
matrices = []
neurons_per_wrg_n = 16
mults_per_thread_n = 16

for result in results:
    matrix = np.zeros((mults_per_thread_n, neurons_per_wrg_n), dtype=np.float32)
    for mults_per_thread in np.arange(mults_per_thread_n):
        for neurons_per_wrg in np.arange(neurons_per_wrg_n):
            matrix[mults_per_thread, neurons_per_wrg] = np.nan_to_num(
                result[(result["neurons_per_wrg"] == neurons_per_wrg) &
                       (result["mults_per_thread"] == mults_per_thread)]
                [["n_inputs_l0_nonpadded", "n_neurons_l0_nonpadded", "runtime_l0"]]
                    .apply(performance, axis=1).mean())
            print(".", end="")

    matrices.append(matrix)
print()

matrices = standardize(matrices)


""" Plot stuff """
import matplotlib.pyplot as plt

second_min_all_exp = 2
max_global = 0
for matrix in matrices:
    matrix2 = np.copy(matrix)
    np.place(matrix2, matrix2 == 0, [2])
    second_min = matrix2.min()
    if second_min < second_min_all_exp:
        second_min_all_exp = second_min
    if matrix.max() > max_global:
        max_global = matrix.max()

i = 0
for matrix in matrices:
    matrix2 = np.copy(matrix)
    np.place(matrix2, matrix2 == 0, [2])
    second_min = matrix2.min()
    # Renders the plot
    _, ax = plt.subplots()
    cmap = plt.cm.viridis

    heatmap = ax.pcolor(matrix, cmap=cmap, vmin=second_min, vmax=matrix.max())
    m = plt.cm.ScalarMappable(cmap=cmap)
    cmap.set_under(color="white")
    m.set_clim(vmin=second_min, vmax=matrix.max())
    m.set_array(matrix)
    plt.colorbar(m)

    ax.set_title("Performance of varying NN configurations")
    ax.set_xlabel("Number of neurons per work group")
    ax.set_ylabel("Number of connections processed per thread")
    step = 2
    ax.set_xticks(np.arange(0, neurons_per_wrg_n+step, step))
    ax.set_xticklabels(np.arange(0, 16+step, step))
    ax.set_yticks(np.arange(0, mults_per_thread_n+step, step))
    ax.set_yticklabels(np.arange(0, 16+step, step))

    # Format
    fig = plt.gcf()
    fig.set_size_inches(10, 7)
    # turn off the frame
    ax.set_frame_on(False)
    print("Second min: %.3f" % second_min)
    matrix_nan = np.copy(matrix)
    matrix_nan[matrix_nan == 0] = np.nan
    print("Max: %.4f" % np.nanmax(matrix_nan))
    print("Mean: %.4f" % np.nanmean(matrix_nan))
    print("Median: %.4f" % np.nanmedian(matrix_nan, axis=None))
    print()

    plt.savefig(os.path.join(imgPath, "plot.png"))
    i += 1