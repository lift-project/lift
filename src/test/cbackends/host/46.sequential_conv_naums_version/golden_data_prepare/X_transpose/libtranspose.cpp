
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
void execute(float * v_initial_param_2921_278, float * & v_user_func_2923_279, int v_input_xdim_266, int v_input_ydim_267, int v_in_channels_268){
    // Allocate memory for output pointers
    v_user_func_2923_279 = reinterpret_cast<float *>(malloc(((v_input_xdim_266 * v_input_ydim_267 * v_in_channels_268) * sizeof(float)))); 
    {
        {
            
        }
    }
    // For each element processed sequentially
    for (int v_i_274 = 0;(v_i_274 <= (-1 + v_in_channels_268)); (++v_i_274)){
        // For each element processed sequentially
        for (int v_i_276 = 0;(v_i_276 <= (-1 + v_input_ydim_267)); (++v_i_276)){
            // For each element processed sequentially
            for (int v_i_277 = 0;(v_i_277 <= (-1 + v_input_xdim_266)); (++v_i_277)){
                v_user_func_2923_279[(v_i_277 + (v_input_xdim_266 * v_input_ydim_267 * v_i_274) + (v_input_xdim_266 * v_i_276))] = id(v_initial_param_2921_278[(v_i_274 + (v_input_xdim_266 * v_in_channels_268 * v_i_276) + (v_in_channels_268 * v_i_277))]); 
            }
        }
    }
}
}; 