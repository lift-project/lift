
//data layout, from outputmost to innermost
// image: #in_channels x height x width
// kernel: #out_channels x in_channels x height x width
// output: #out_channels x output_height x output_width

// layer 2
#define Input_channels 64
#define Original_input_height 112
#define Original_input_width 112
#define Input_height 116
#define Input_width 116
#define Padded_Input_width 116 // extra 2 px of padding
#define Kernel_height 3
#define Kernel_width 3
#define Num_kernels 128
// tile size = 5, tile step = 3, n tiles = 38
#define Output_height 114
#define Output_width 114
// tile size (input channels) = 32, n tiles = 2
#define Original_output_height 112
#define Original_output_width 112




// each thread update 4 continous elements, this gives 29 threads for this kernel for each channel
// the last thread only do two, this gives control divergence for the last thread block, it should be ok.
// can try with vload and vstore, or without, this makes two variants.
// need to schedule 64 as second dimension for number of channels
kernel void PADDING_1( global float * restrict output
			      ){
	size_t const line_offset = get_global_id(0) * 4;
	size_t const channel_offset = get_global_id(1) * Input_height * Input_width;


	vstore4( (float4)(0.0f,0.0f,0.0f,0.0f), 0, output + channel_offset + line_offset);
	if(get_global_id(0) == 28)
		output[channel_offset + line_offset + 4] = 0;
}

// each thread update 4 continous elements, this gives 28 threads for this kernel for each line,
// the last thread handles fills one more zero, which is first zero on next line
// 28, 112, 64
kernel void PADDING_2(const global float * restrict input,
			      global float * restrict output
			      ){

	size_t const in_channel_offset = get_global_id(2) * Original_input_height * Original_input_width;
	size_t const in_lines_offset = get_global_id(1) * Original_input_width;
	size_t const in_line_offset = get_global_id(0) * 4;

	size_t const out_channel_offset = get_global_id(2) * Input_height * Input_width;
	size_t const out_lines_offset = (get_global_id(1) + 1) * Input_width;
	size_t const out_line_offset = get_global_id(0) * 4 + 1 ;
	size_t const total_offset = out_channel_offset + out_lines_offset + out_line_offset;
	
	
	float4 in_out = vload4( 0, input + in_channel_offset + in_lines_offset + in_line_offset);
	vstore4(in_out, 0, output + total_offset); 
	if(get_global_id(0) == 27 )
		vstore4( (float4)(0.0f, 0.0f, 0.0f, 0.0f), 0, output + total_offset + 4);
		
}

// each thread update 4 continous elements, this gives 29 threads for this kernel for each channel
// can try with vload and vstore, or without, this makes two variants.
kernel void PADDING_3( global float * restrict output
			      ){

	size_t const line_offset = (Original_input_height + get_global_id(1) + 1) * Input_width + get_global_id(0) * 4;
	size_t const channel_offset = get_global_id(2) * Input_height * Input_width;

	vstore4( (float4)(0.0f,0.0f,0.0f,0.0f), 0, output + channel_offset + line_offset);
}


//data layout, from outputmost to innermost
// image: #in_channels x height x width
// spawn #in_channels threads
kernel void PADDING(const global float * restrict input,
			      global float * restrict output
			      ) {

	size_t const in_offset = get_global_id(0) * Original_input_height * Original_input_width;
	size_t out_offset = get_global_id(0) * (Original_input_height + 2 ) * ( Original_input_width + 4 );

	for(int i = 0; i< (Original_input_width + 4); ++i)
		output[out_offset + i] = 0.0f;
	
	out_offset += (Original_input_width + 4);

	for(int r = 0; r < Original_input_height; ++r){
		output[out_offset++] = 0;
		for(int c = 0; c < Original_input_width; ++c )
		{
			output[out_offset++] = input[ in_offset + r * Original_input_width + c ];
		}
		output[out_offset++] = 0;
		output[out_offset++] = 0;
		output[out_offset++] = 0;
	}


	for(int i = 0; i< (Original_input_width + 4); ++i)
		output[ out_offset + i] = 0.0f;
	

}


// spawn Num_kernels * Output_height * Output_width number of threads
// 112, 112, 128
kernel void REDUCTION(const global float * restrict input,
			      global float * restrict output
			      ) {

	int const in_offset0 = get_global_id(2) * Output_height * Output_width + get_global_id(1) * Output_width + get_global_id(0);
	int const in_offset1 = in_offset0 + Num_kernels * Output_height * Output_width;

	int const out_offset = get_global_id(2) * Original_output_height * Original_output_width 
				+ get_global_id(1) * Original_output_width + get_global_id(0);

	output[out_offset] = ( input[in_offset0] + input[in_offset1] );

}



//-------------------------------
// dim0 to dim2: (76, 38, 128); without last row: (76, 37, 128)
// (nTilesSpatial /* x */ * nTilesInCh, nTilesSpatial /* y */, out_channels)
// Each thread processes (tileSizeX x tileSizeY x tileSizeInCh)(5 x 5 x 32) input elements, i.e. 3 x 3 sliding windows
// Each thred processes 1 kernel
kernel void KERNEL(const global float * restrict input,
			      const global float * restrict weights,
			      //const global float * restrict biases,
			      global float * restrict output
			      ) {


const size_t kx = get_global_id(2); // output_channel id
const size_t wy = get_global_id(1); // wy pure tile id across spatial Y 
size_t wx = get_global_id(0); // wx covers tile id across spatial X (x, size 5, i.e. 38 tiles), tile id across InCh (in channels, size 2)


size_t const which_portion = wx / 38 ; // input channel tile id
wx = wx - 38 * which_portion ;
float o0_0 = 0.0f; //1 tile per thread, for tile size of 5, means 3x3 outputs
float o0_1 = 0.0f;
float o0_2 = 0.0f;
float o1_0 = 0.0f;
float o1_1 = 0.0f;
float o1_2 = 0.0f;
float o2_0 = 0.0f;
float o2_1 = 0.0f;
float o2_2 = 0.0f;
size_t const output_offset0 = kx * Output_height * Output_width + (3  * wy + 0 ) * Output_width + 3 * wx + which_portion * Num_kernels * Output_height * Output_width + 0 + 0 * Output_width; // row 0
size_t const output_offset1 = kx * Output_height * Output_width + (3  * wy + 1 ) * Output_width + 3 * wx + which_portion * Num_kernels * Output_height * Output_width + 0 + 0 * Output_width; // row 1
size_t const output_offset2 = kx * Output_height * Output_width + (3  * wy + 2 ) * Output_width + 3 * wx + which_portion * Num_kernels * Output_height * Output_width + 0 + 0 * Output_width; // row 2
// cache data
// sliding window size 3 x 3 x (num of input channels per thread=2)
//    sliding window size 3 x 3 x (num of input channels per thread=2)
for(int icx = 0 + which_portion * 32 ; icx < (which_portion+1) * 32; ++icx){// icx = input channel id. The loop does nTilesInCh iterations
    size_t const input_offset = icx * Input_height * Padded_Input_width + (3 * wy) * Padded_Input_width + 3 * wx + 0 + 0 * Padded_Input_width; // (3 * wy) = current sliding window ID
    size_t const weight_offset = kx * Input_channels * Kernel_height * Kernel_width + icx * Kernel_height * Kernel_width ;

    float4 const wv0 = vload4(0, weights + weight_offset); // first 4 kernel weights
    float4 const wv1 = vload4(1, weights + weight_offset); // second 4 kernel weights
    float const w8 = weights[weight_offset + 8];           // the last kernel weight 


    float4 const v0_0=vload4( 0 , input + input_offset + 0 * Padded_Input_width + 0 * 4 ); // first 4 elements of tile row 0
    float const v0_1= input [ input_offset + 0 * Padded_Input_width + 1 * 4 ];             // last 1 element of tile row 0
    float4 const v1_0=vload4( 0 , input + input_offset + 1 * Padded_Input_width + 0 * 4 ); // first 4 elements of tile row 1
    float const v1_1= input [ input_offset + 1 * Padded_Input_width + 1 * 4 ];             // last 1 element of tile row 1
    float4 const v2_0=vload4( 0 , input + input_offset + 2 * Padded_Input_width + 0 * 4 ); // first 4 elements of tile row 2
    float const v2_1= input [ input_offset + 2 * Padded_Input_width + 1 * 4 ];             // last 1 element of tile row 2
    float4 const v3_0=vload4( 0 , input + input_offset + 3 * Padded_Input_width + 0 * 4 ); // first 4 elements of tile row 3
    float const v3_1= input [ input_offset + 3 * Padded_Input_width + 1 * 4 ];             // last 1 element of tile row 3
    float4 const v4_0=vload4( 0 , input + input_offset + 4 * Padded_Input_width + 0 * 4 ); // first 4 elements of tile row 4
    float const v4_1= input [ input_offset + 4 * Padded_Input_width + 1 * 4 ];             // last 1 element of tile row 4

    o0_0 += wv0.s0 * v0_0.s0; // tile window 0, 0
    o0_0 += wv0.s1 * v0_0.s1;
    o0_0 += wv0.s2 * v0_0.s2;
    o0_0 += wv0.s3 * v1_0.s0;
    o0_0 += wv1.s0 * v1_0.s1;
    o0_0 += wv1.s1 * v1_0.s2;
    o0_0 += wv1.s2 * v2_0.s0;
    o0_0 += wv1.s3 * v2_0.s1;
    o0_0 += w8 * v2_0.s2;
    o0_1 += wv0.s0 * v0_0.s1; // tile window 1, 0
    o0_1 += wv0.s1 * v0_0.s2;
    o0_1 += wv0.s2 * v0_0.s3;
    o0_1 += wv0.s3 * v1_0.s1;
    o0_1 += wv1.s0 * v1_0.s2;
    o0_1 += wv1.s1 * v1_0.s3;
    o0_1 += wv1.s2 * v2_0.s1;
    o0_1 += wv1.s3 * v2_0.s2;
    o0_1 += w8 * v2_0.s3;
    o0_2 += wv0.s0 * v0_0.s2; // tile window 2, 0
    o0_2 += wv0.s1 * v0_0.s3;
    o0_2 += wv0.s2 * v0_1;
    o0_2 += wv0.s3 * v1_0.s2;
    o0_2 += wv1.s0 * v1_0.s3;
    o0_2 += wv1.s1 * v1_1;
    o0_2 += wv1.s2 * v2_0.s2;
    o0_2 += wv1.s3 * v2_0.s3;
    o0_2 += w8 * v2_1;

    o1_0 += wv0.s0 * v1_0.s0; // tile window 0, 1
    o1_0 += wv0.s1 * v1_0.s1;
    o1_0 += wv0.s2 * v1_0.s2;
    o1_0 += wv0.s3 * v2_0.s0;
    o1_0 += wv1.s0 * v2_0.s1;
    o1_0 += wv1.s1 * v2_0.s2;
    o1_0 += wv1.s2 * v3_0.s0;
    o1_0 += wv1.s3 * v3_0.s1;
    o1_0 += w8 * v3_0.s2;
    o1_1 += wv0.s0 * v1_0.s1; // tile window 1, 1
    o1_1 += wv0.s1 * v1_0.s2;
    o1_1 += wv0.s2 * v1_0.s3;
    o1_1 += wv0.s3 * v2_0.s1;
    o1_1 += wv1.s0 * v2_0.s2;
    o1_1 += wv1.s1 * v2_0.s3;
    o1_1 += wv1.s2 * v3_0.s1;
    o1_1 += wv1.s3 * v3_0.s2;
    o1_1 += w8 * v3_0.s3;
    o1_2 += wv0.s0 * v1_0.s2; // tile window 2, 1
    o1_2 += wv0.s1 * v1_0.s3;
    o1_2 += wv0.s2 * v1_1;
    o1_2 += wv0.s3 * v2_0.s2;
    o1_2 += wv1.s0 * v2_0.s3;
    o1_2 += wv1.s1 * v2_1;
    o1_2 += wv1.s2 * v3_0.s2;
    o1_2 += wv1.s3 * v3_0.s3;
    o1_2 += w8 * v3_1;

    o2_0 += wv0.s0 * v2_0.s0; // tile window 0, 2
    o2_0 += wv0.s1 * v2_0.s1;
    o2_0 += wv0.s2 * v2_0.s2;
    o2_0 += wv0.s3 * v3_0.s0;
    o2_0 += wv1.s0 * v3_0.s1;
    o2_0 += wv1.s1 * v3_0.s2;
    o2_0 += wv1.s2 * v4_0.s0;
    o2_0 += wv1.s3 * v4_0.s1;
    o2_0 += w8 * v4_0.s2;
    o2_1 += wv0.s0 * v2_0.s1; // tile window 1, 2
    o2_1 += wv0.s1 * v2_0.s2;
    o2_1 += wv0.s2 * v2_0.s3;
    o2_1 += wv0.s3 * v3_0.s1;
    o2_1 += wv1.s0 * v3_0.s2;
    o2_1 += wv1.s1 * v3_0.s3;
    o2_1 += wv1.s2 * v4_0.s1;
    o2_1 += wv1.s3 * v4_0.s2;
    o2_1 += w8 * v4_0.s3;
    o2_2 += wv0.s0 * v2_0.s2; // tile window 2, 2
    o2_2 += wv0.s1 * v2_0.s3;
    o2_2 += wv0.s2 * v2_1;
    o2_2 += wv0.s3 * v3_0.s2;
    o2_2 += wv1.s0 * v3_0.s3;
    o2_2 += wv1.s1 * v3_1;
    o2_2 += wv1.s2 * v4_0.s2;
    o2_2 += wv1.s3 * v4_0.s3;
    o2_2 += w8 * v4_1;

}
vstore3( (float3) (o0_0, o0_1, o0_2) , 0, output + output_offset0 + 4 * 0);
vstore3( (float3) (o1_0, o1_1, o1_2) , 0, output + output_offset1 + 4 * 0);
vstore3( (float3) (o2_0, o2_1, o2_2) , 0, output + output_offset2 + 4 * 0);
} 




