# ----------------------------------------------------------------
# ONNX model construction

import onnx
import onnx.utils
from onnx import helper,TensorProto, shape_inference

conv_node = helper.make_node(
    'Conv',
    inputs = ['X', 'W'],
    outputs = ['Y'],
    kernel_shape = [$kernelChannels%d, $kernelWidthHeight%d, $kernelWidthHeight%d],
    pads = [$padWidthHeight%d, $padWidthHeight%d],
    dilations = [0, 0],
    # default values, optional:
    auto_pad = "NOTSET",
    strides = [$padWidthHeight%d, $padWidthHeight%d],
    group = 1)

conv_graph = helper.make_graph(
    [conv_node],
    'single_conv_net_0_graph',
    [helper.make_tensor_value_info('X', TensorProto.FLOAT,
                                   ($nInputs%d, $inputChannels%d,
                                    $inputWidthHeight%d, $inputWidthHeight%d)),
     helper.make_tensor_value_info('W', TensorProto.FLOAT,
                                   ($kernelChannels%d,
                                    $inputChannels%d,
                                    $kernelWidthHeight%d,
                                    $kernelWidthHeight%d))],
    [helper.make_tensor_value_info('Y', TensorProto.FLOAT,
                                   ($nInputs%d,
                                    $kernelChannels%d,
                                    $outputWidthHeight%d,
                                    $outputWidthHeight%d))])

conv_model = helper.make_model(conv_graph,
                               producer_name='single_conv_net_0_producer')

onnx.checker.check_model(conv_model)

onnx.save(conv_model, "$onnxConfigPath%s")