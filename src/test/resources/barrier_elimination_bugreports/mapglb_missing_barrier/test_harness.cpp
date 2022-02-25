#define CL_HPP_TARGET_OPENCL_VERSION 200
#define CL_TARGET_OPENCL_VERSION 200
#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <stdlib.h>
#include "./libconv.cpp"
#include "../layerconfig.cpp"

float mult(float l, float r){
    {
        { return l * r; }; 
    }
}
float add(float x, float y){
    {
        { return x+y; }; 
    }
}
float id(float x){
    {
        { return x; }; 
    }
}

void print_time() {
    using std::chrono::system_clock;
    auto currentTime = std::chrono::system_clock::now();
    char buffer[80];

    auto transformed = currentTime.time_since_epoch().count() / 1000000;

    auto millis = transformed % 1000;

    std::time_t tt;
    tt = system_clock::to_time_t ( currentTime );
    auto timeinfo = localtime (&tt);
    strftime (buffer,80,"%F %H:%M:%S",timeinfo);
    sprintf(buffer, "%s:%03d",buffer,(int)millis);
    std::cout << std::string(buffer) << std::endl;
}



int main( int argc, char** argv ) {
	assert(argc == 2);
	const std::string kernel_path(argv[1]);

	lift_init(kernel_path);


    // const int input_pad = 1;

    // const int kernel_xdim_SV = 3;
    // const int kernel_ydim_SV = 3;
    // const int kernel_stride = 3;
    // const int input_xdim_SV = 224;
    // const int input_ydim_SV = input_xdim_SV;
    // const int in_channels_SV = 64;
    // const int out_channels_SV = 64;
    // const int n_inputs_SV = 1;
    const int input_WH_padded = layerConfig.input_WH + layerConfig.input_pad*2;
    const int output_WH = (input_WH_padded - (layerConfig.kernel_WH - layerConfig.kernel_stride)) / layerConfig.kernel_stride;
    const unsigned int platform_id = 0, device_id = 0;

    /* float input_X[n_inputs_SV][input_ydim_SV][input_xdim_SV][in_channels_SV] = ; */
    vector<float> input_X(layerConfig.n_inputs * input_WH_padded * input_WH_padded * layerConfig.in_channels, 0.0f);

    ifstream random_x_file("/home/shunya/naums/hipeac2020/randomeX/vgg/"+std::to_string(layerConfig.layer_id)+"/random_x_padded.txt");
    if (!random_x_file.is_open()) {
        std::cerr << "Couldn't open the sample input data file. Exiting.\n";
        return( 1 );
    }
    istream_iterator<float> start_x(random_x_file), end_x;
    copy(start_x, end_x, input_X.begin());

     /* float input_B[out_channels_SV] = {0.0, 1.0, 2.0}; */
    // vector<float> input_B(layerConfig.out_channels, 1.58f);

     /* float input_K[out_channels_SV][kernel_ydim_SV][kernel_xdim_SV][in_channels_SV] = ; */
    vector<float> input_K(layerConfig.out_channels * layerConfig.kernel_WH * layerConfig.kernel_WH * layerConfig.in_channels, 3.198f);

    /* float out[n_inputs_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)][out_channels_SV] = {0}; */

    float *out1 = nullptr;

    /* const int count = 3; */
    /* for(int i = 0; i < count; ++i){ */
    print_time();
    // lift::execute(input_K.data(), input_B.data(), input_X.data(), out1);
    lift::execute(input_X.data(), input_K.data(), out1);
    print_time();
        /* execute(input_X.data(), out1); */
    /* } */


    
    float (&out)[layerConfig.n_inputs][layerConfig.out_channels][output_WH][output_WH] = 
        *reinterpret_cast<float (*)[layerConfig.n_inputs][layerConfig.out_channels][output_WH][output_WH]>(out1);

    unsigned golden_data_size = layerConfig.n_inputs * layerConfig.out_channels * output_WH * output_WH;
    cout<<"golden_data_size = "<< golden_data_size << endl;

    vector<float> golden_data(golden_data_size, -999.99);

    ifstream file("../data.txt");

    if (!file.good()) {
       fprintf(stderr, "Could not open the data file.\n");
       return( 1 );
    }

    istream_iterator<float> start(file), end;
    copy(start, end, golden_data.begin());

    float (&gold)[layerConfig.n_inputs][layerConfig.out_channels][output_WH][output_WH] = 
        *reinterpret_cast<float (*)[layerConfig.n_inputs][layerConfig.out_channels][output_WH][output_WH]>(golden_data.data());



/* #include "./golden.c" */

    // Verify the result
    bool result=true;

    int counter = 0; 

    if (false == false)
        for (int i=0; i < layerConfig.n_inputs; i++) {
            for (int j=0; j < layerConfig.out_channels; j++) {
                for (int k=0; k < output_WH; k++) {
                    for (int l=0; l < output_WH; l++) {
                        if ( (out[i][j][k][l] == 0 && gold[i][j][k][l] != 0) ||
                             (out[i][j][k][l] != 0 && gold[i][j][k][l] == 0) ||
                             (gold[i][j][k][l] != 0 && out[i][j][k][l] != 0 && (abs(out[i][j][k][l] - gold[i][j][k][l]) / gold[i][j][k][l]) > 0.001 ) ) {
                            result=false;
                            fprintf(stderr, "out[%d][%d][%d][%d] = ", i, j, k, l);
                            fprintf(stderr, "%f != %f\n", out[i][j][k][l], gold[i][j][k][l]);
                            for (int y=0; y < 100; y++) {
                                fprintf(stderr, "%f ", out[i][j][k][l + y]);
                            }

                //if(++counter == 10) {
                            fprintf(stderr, "\nFail.\n");
                            return( 1 );
                //}
                        }
                    }
                }
            }
        }
    else
        cout <<"SKIPPING VERIFICATION" << endl;

    std::cerr<< "Success!\n";

    std::cerr << "Done.\n";
    return( EXIT_SUCCESS );
}
