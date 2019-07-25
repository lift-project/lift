
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ID_H
#define ID_H
; 
float id(float x){
    { return x; }; 
}

#endif
 ; 
void execute(float * v_initial_param_2953_310, float * & v_user_func_2955_311, int v_kernel_h_264, int v_kernel_w_265, int v_in_channels_268, int v_out_channels_269){
    // Allocate memory for output pointers
    v_user_func_2955_311 = reinterpret_cast<float *>(malloc(((v_kernel_h_264 * v_kernel_w_265 * v_in_channels_268 * v_out_channels_269) * sizeof(float)))); 
    {
        {
            {
                
            }
        }
    }
    // For each element processed sequentially
    for (int v_i_304 = 0;(v_i_304 <= (-1 + v_out_channels_269)); (++v_i_304)){
        // For each element processed sequentially
        for (int v_i_307 = 0;(v_i_307 <= (-1 + v_in_channels_268)); (++v_i_307)){
            // For each element processed sequentially
            for (int v_i_308 = 0;(v_i_308 <= (-1 + v_kernel_h_264)); (++v_i_308)){
                // For each element processed sequentially
                for (int v_i_309 = 0;(v_i_309 <= (-1 + v_kernel_w_265)); (++v_i_309)){
                    v_user_func_2955_311[(v_i_309 + (v_kernel_h_264 * v_kernel_w_265 * v_in_channels_268 * v_i_304) + (v_kernel_h_264 * v_kernel_w_265 * v_i_307) + (v_kernel_w_265 * v_i_308))] = id(v_initial_param_2953_310[(v_i_304 + (v_kernel_w_265 * v_in_channels_268 * v_out_channels_269 * v_i_308) + (v_in_channels_268 * v_out_channels_269 * v_i_309) + (v_out_channels_269 * v_i_307))]); 
                }
            }
        }
    }
}
}; 