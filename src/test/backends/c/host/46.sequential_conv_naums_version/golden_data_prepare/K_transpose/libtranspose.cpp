
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
void execute(float * v_initial_param_6897_2721, float * & v_user_func_6899_2722, int v_kernel_h_2675, int v_kernel_w_2676, int v_in_channels_2679, int v_out_channels_2680){
    // Allocate memory for output pointers
    v_user_func_6899_2722 = reinterpret_cast<float *>(malloc(((v_kernel_h_2675 * v_kernel_w_2676 * v_in_channels_2679 * v_out_channels_2680) * sizeof(float)))); 
    {
        {
            {
                
            }
        }
    }
    // For each element processed sequentially
    for (int v_i_2715 = 0;(v_i_2715 <= (-1 + v_out_channels_2680)); (++v_i_2715)){
        // For each element processed sequentially
        for (int v_i_2718 = 0;(v_i_2718 <= (-1 + v_in_channels_2679)); (++v_i_2718)){
            // For each element processed sequentially
            for (int v_i_2719 = 0;(v_i_2719 <= (-1 + v_kernel_h_2675)); (++v_i_2719)){
                // For each element processed sequentially
                for (int v_i_2720 = 0;(v_i_2720 <= (-1 + v_kernel_w_2676)); (++v_i_2720)){
                    v_user_func_6899_2722[(v_i_2720 + (v_kernel_h_2675 * v_kernel_w_2676 * v_in_channels_2679 * v_i_2715) + (v_kernel_h_2675 * v_kernel_w_2676 * v_i_2718) + (v_kernel_w_2676 * v_i_2719))] = id(v_initial_param_6897_2721[(v_i_2715 + (v_kernel_w_2676 * v_in_channels_2679 * v_out_channels_2680 * v_i_2719) + (v_in_channels_2679 * v_out_channels_2680 * v_i_2720) + (v_out_channels_2680 * v_i_2718))]); 
                }
            }
        }
    }
}
}; 