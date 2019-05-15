
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef FLOOR_UF_H
#define FLOOR_UF_H
; 
float floor_uf(float x){
    return floor(x);; 
}

#endif; 
void floor(float * v_initial_param_234_106, float * & v_user_func_236_107, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_236_107 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_105 = 0;(v_i_105 <= (-1 + v_N_0)); (++v_i_105)){
        v_user_func_236_107[v_i_105] = floor_uf(v_initial_param_234_106[v_i_105]); 
    }
}
}; 