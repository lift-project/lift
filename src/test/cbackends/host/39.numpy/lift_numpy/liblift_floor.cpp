
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
void lift_floor(float * v_initial_param_3273_512, float * & v_user_func_3275_513, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3275_513 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_511 = 0;(v_i_511 <= (-1 + v_N_352)); (++v_i_511)){
        v_user_func_3275_513[v_i_511] = floor_uf(v_initial_param_3273_512[v_i_511]); 
    }
}
}; 