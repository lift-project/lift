import numpy as np
from collections import namedtuple
import os
import pandas as pd
import random

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


# def load_experiments():
#     """
#     Loads Lift results.
#     :return: Experiment results
#     """
#     expDirAbs = os.path.abspath("../mlp/")  # Experiments directory
#     results_lift = None
#     i = 0
#     dirs = [Directory(path=expDirAbs, name=d) for d in filter(
#         lambda d: os.path.isdir(os.path.join(expDirAbs, d)) and d.startswith("experiment.") and
#                   os.path.isdir(os.path.join(expDirAbs, d, "results_lift")),
#         os.listdir(expDirAbs))]
#
#     for eDir in dirs:
#         resultsLiftDir = os.path.join(eDir.path, eDir.name, "results_lift")
#         print(eDir)
#
#         for filename in os.listdir(resultsLiftDir):
#             single_csv = pd.read_csv(os.path.join(resultsLiftDir, filename), index_col=False)
#             single_csv["datetime"] = filename[:filename.find("_")]
#             if results_lift is None:
#                 results_lift = single_csv
#             else:
#                 results_lift = results_lift.append(single_csv, ignore_index=True)
#             i += 1
#             if i % 20 == 0:
#                 print(".", end="")
#         print()
#     return results_lift


# nonfiltered_results = load_experiments()
# if nonfiltered_results is None:
#     print("No results found!")
#     quit()
#
# print("%d non-filtered results." % len(nonfiltered_results))

""" Filter results """
# results = [nonfiltered_results[np.all([key in nonfiltered_results and
#                                        nonfiltered_results[key].isin(values)
#                                        for key, values in config.items()], axis=0)]
#            for config in experiment_configs]
n_kernels_max = 20
raw_data = {"runtime_l0": [],
            "nKernelsPerWrg": [],
            "tileSize": [],
            "seqPerThread": []}
nKernelsTotal = 64
for seqPerThread in range(1, 8+1):
    for nKernelsPerWrg in range(1, 32+1):
        raw_data["seqPerThread"].append(seqPerThread)
        raw_data["nKernelsPerWrg"].append(nKernelsPerWrg)
        tileSize = np.maximum(16.0, np.floor(np.sqrt(1024 / (np.ceil(16 * nKernelsPerWrg / seqPerThread) * 16)) + 16 - 1))
        raw_data["tileSize"].append(tileSize)
        tileStep = tileSize - (16 - 1)
        outerLength = np.ceil((28 - (tileSize - tileStep)) / tileStep)
        loc0 = np.ceil(16 * nKernelsPerWrg / seqPerThread)
        loc1 = np.power(tileSize - (16 - 1), 2)
        loc2 = 16
        groupSize = loc0 * loc1 * loc2
        numberOfGroups = np.ceil(nKernelsTotal / nKernelsPerWrg) * outerLength
        duplicateReads = numberOfGroups * (tileSize - tileStep)
        raw_data["runtime_l0"].append((duplicateReads + 600 * (random.random() - 0.5)) / 100)

results = [pd.DataFrame.from_dict(raw_data)]

print("Loaded " + str(len(results)) + " experiment(s):")
for exp_i in range(len(results)):
    print("Experiment " + str(exp_i) + ": " + str(len(results[exp_i])) + " records")


""" Format data """
# Runtimes: s -> ms
original_status = pd.options.mode.chained_assignment
pd.options.mode.chained_assignment = None
# for result in results:
#     result.loc[:, "runtime_l0"] = result["runtime_l0"].apply(lambda x: x * 1000)
#     result.loc[:, "runtime_l1"] = result["runtime_l1"].apply(lambda x: x * 1000)
#     result.loc[:, "runtime_l2"] = result["runtime_l2"].apply(lambda x: x * 1000)
pd.options.mode.chained_assignment = original_status
print("Data was formatted.")
data_is_reset = False


def performance(df):
    """

    :param df:
    :return:
    """
    return df["runtime_l0"] # (df["n_inputs_l0_nonpadded"] * df["n_neurons_l0_nonpadded"]) / df["runtime_l0"] if not df.empty else 0


def standardize(matrices):
    """
    Scale values to the range from 0 to 1
    :param matrices:
    :return:
    """
    global_max = 0
    global_min = 999

    for i in range(len(matrices)):
        if np.nanmax(matrices[i]) > global_max:
            global_max = np.nanmax(matrices[i])
        if np.nanmin(matrices[i]) < global_min:
            global_min = np.nanmin(matrices[i])
    for i in range(len(matrices)):
        matrices[i] = ((matrices[i] - global_min) * (-1) + (global_max - global_min)) / (global_max - global_min)
    return matrices


""" Prepare data """
matrices = []
tileSizes_n = 5
tileSizes = [16, 17, 18, 19, 20]
nKernelsPerWrg_n = 32

for result in results:
    matrix = np.zeros((nKernelsPerWrg_n, tileSizes_n), dtype=np.float32)
    for nKernelsPerWrg in range(1, nKernelsPerWrg_n+1):
        for tileSize_i in range(1, tileSizes_n+1):
            matrix[nKernelsPerWrg-1, tileSize_i-1] = (
                result[(result["tileSize"] == tileSizes[tileSize_i-1]) &
                       (result["nKernelsPerWrg"] == nKernelsPerWrg) &
                       (result["seqPerThread"] == 8)]
                [["runtime_l0"]]
                    .apply(performance, axis=1).mean())
            print(".", end="")

    matrices.append(matrix)
print()

matrices = standardize(matrices)


""" Plot stuff """
import matplotlib.pyplot as plt

i = 0
for matrix in matrices:
    global_max = np.nanmax(matrix)
    global_min = np.nanmin(matrix)
    offset = 0.1
    matrix = matrix + offset
    np.place(matrix, np.isnan(matrix), [0])
    matrix = matrix - offset
    # Renders the plot
    _, ax = plt.subplots()
    cmap = plt.cm.viridis

    heatmap = ax.pcolor(matrix, cmap=cmap, vmin=global_min, vmax=global_max)
    m = plt.cm.ScalarMappable(cmap=cmap)
    cmap.set_under(color="white")
    m.set_clim(vmin=global_min, vmax=global_max)
    m.set_array(matrix)
    plt.colorbar(m)

    ax.set_title("Relative efficiency of various parallelization mappings")
    ax.set_xlabel("Image tile size, pixels")
    ax.set_ylabel("Number of kernels per workgroup, kernels")
    step = 1
    ax.set_xticks(np.arange(0, 5, 1))
    #ax.set_xticklabels(np.arange(0, 16+step, step))
    ax.set_xticklabels(np.arange(16, 21, 1))
    ax.set_yticks(np.arange(0, nKernelsPerWrg_n + step, step))
    ax.set_yticklabels(np.arange(1, 1+nKernelsPerWrg_n+step, step))

    # Format
    fig = plt.gcf()
    fig.set_size_inches(10, 7)
    # turn off the frame
    ax.set_frame_on(False)
    print("Second min: %.3f" % global_min)
    matrix_nan = np.copy(matrix)
    matrix_nan[matrix_nan == 0] = np.nan
    print("Max: %.4f" % np.nanmax(matrix_nan))
    print("Mean: %.4f" % np.nanmean(matrix_nan))
    print("Median: %.4f" % np.nanmedian(matrix_nan, axis=None))
    print()

    plt.savefig(os.path.join(imgPath, "plot.png"))
    i += 1