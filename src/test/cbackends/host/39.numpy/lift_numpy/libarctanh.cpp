
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
float arctanh_uf(float x){
    { return atanh(x); }; 
}

#endif
 ; 
void arctanh(float * v_initial_param_3245_497, float * & v_user_func_3247_498, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3247_498 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_496 = 0;(v_i_496 <= (-1 + v_N_352)); (++v_i_496)){
        v_user_func_3247_498[v_i_496] = arctanh_uf(v_initial_param_3245_497[v_i_496]); 
    }
}
}; 