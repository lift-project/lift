
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MAXIMUM_UF_H
#define MAXIMUM_UF_H
; 
float maximum_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void maximum(float * v_initial_param_908_400, float * v_initial_param_909_401, float * & v_user_func_915_403, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_915_403 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_399 = 0;(v_i_399 <= (-1 + v_N_0)); (++v_i_399)){
        v_user_func_915_403[v_i_399] = maximum_uf(v_initial_param_908_400[v_i_399], v_initial_param_909_401[v_i_399]); 
    }
}
}; 