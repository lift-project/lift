
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef CEIL_UF_H
#define CEIL_UF_H
; 
float ceil_uf(float x){
    return ceil(x);; 
}

#endif; 
void ceil(float * v_initial_param_241_106, float * & v_user_func_243_107, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_243_107 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_105 = 0;(v_i_105 <= (-1 + v_N_0)); (++v_i_105)){
        v_user_func_243_107[v_i_105] = ceil_uf(v_initial_param_241_106[v_i_105]); 
    }
}
}; 