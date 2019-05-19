
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_UF_H
#define FLOOR_UF_H
; 
float floor_uf(float x){
    return floor(x);; 
}

#endif
 ; 
void lift_floor(float * v_initial_param_277_158, float * & v_user_func_279_159, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_279_159 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_157 = 0;(v_i_157 <= (-1 + v_N_0)); (++v_i_157)){
        v_user_func_279_159[v_i_157] = floor_uf(v_initial_param_277_158[v_i_157]); 
    }
}
}; 