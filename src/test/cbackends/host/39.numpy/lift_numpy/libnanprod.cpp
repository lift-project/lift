
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef PROD2_H
#define PROD2_H
; 
float prod2(float l, float r){
    { return (l * r); }; 
}

#endif; 
void nanprod(float * v_initial_param_255_123, float * & v_user_func_258_124, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_258_124 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_258_124[0] = 1.0f; 
    for (int v_i_122 = 0;(v_i_122 <= (-1 + v_N_0)); (++v_i_122)){
        v_user_func_258_124[0] = prod2(v_user_func_258_124[0], v_initial_param_255_123[v_i_122]); 
    }
}
}; 