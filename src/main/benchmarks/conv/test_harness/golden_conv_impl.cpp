#include <bits/stdc++.h>

using namespace std;


void golden_convolution(float const *input,
		float const *weights,
//		float const *biases,
		float *output,
		int const input_channels,
		int const input_height,
		int const input_width,
		int const kernel_height,
		int const kernel_width,
		int const kernel_stride_y,
		int const kernel_stride_x,
		int const num_kernels) {


	int const output_width = input_width - kernel_width + 1;
	int const output_height = input_height - kernel_height + 1;
	int x,y, input_idx, weight_idx, input_offset, weight_offset;


	for(int kx = 0; kx < num_kernels; ++kx){
		for(int wy=0; wy < (input_height-kernel_height+kernel_stride_y) / kernel_stride_y; wy++){
			for(int wx=0; wx < (input_width-kernel_width+kernel_stride_x) / kernel_stride_x; wx++) {
				float dot_product = 0; //biases[kx];
				for(int icx = 0; icx < input_channels; ++icx){
					for(int ey=0;ey<kernel_height;ey++) {
						for(int ex=0;ex<kernel_width;ex++) {
							x = wx * kernel_stride_x + ex;
							y = wy * kernel_stride_y + ey;
							input_idx = y * input_width + x;
							weight_idx = ey * kernel_width + ex;
							input_offset = icx * input_height * input_width;
							weight_offset =
								kx * input_channels * kernel_height * kernel_width
								+ icx * kernel_height * kernel_width ;



							dot_product += input[ input_offset + input_idx ]
								* weights[ weight_offset + weight_idx ];
						}
					}
				}
				output[ kx * output_height * output_width + wy * output_width + wx  ] = dot_product;
			}
		}

	}



}
