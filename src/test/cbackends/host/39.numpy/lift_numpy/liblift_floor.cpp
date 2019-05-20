
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
void lift_floor(float * v_initial_param_277_160, float * & v_user_func_279_161, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_279_161 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_159 = 0;(v_i_159 <= (-1 + v_N_0)); (++v_i_159)){
        v_user_func_279_161[v_i_159] = floor_uf(v_initial_param_277_160[v_i_159]); 
    }
}
}; 