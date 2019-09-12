
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
void execute(float * v_initial_param_6865_2689, float * & v_user_func_6867_2690, int v_input_xdim_2677, int v_input_ydim_2678, int v_in_channels_2679){
    // Allocate memory for output pointers
    v_user_func_6867_2690 = reinterpret_cast<float *>(malloc(((v_input_xdim_2677 * v_input_ydim_2678 * v_in_channels_2679) * sizeof(float)))); 
    {
        {
            
        }
    }
    // For each element processed sequentially
    for (int v_i_2685 = 0;(v_i_2685 <= (-1 + v_in_channels_2679)); (++v_i_2685)){
        // For each element processed sequentially
        for (int v_i_2687 = 0;(v_i_2687 <= (-1 + v_input_ydim_2678)); (++v_i_2687)){
            // For each element processed sequentially
            for (int v_i_2688 = 0;(v_i_2688 <= (-1 + v_input_xdim_2677)); (++v_i_2688)){
                v_user_func_6867_2690[(v_i_2688 + (v_input_xdim_2677 * v_input_ydim_2678 * v_i_2685) + (v_input_xdim_2677 * v_i_2687))] = id(v_initial_param_6865_2689[(v_i_2685 + (v_input_xdim_2677 * v_in_channels_2679 * v_i_2687) + (v_in_channels_2679 * v_i_2688))]); 
            }
        }
    }
}
}; 