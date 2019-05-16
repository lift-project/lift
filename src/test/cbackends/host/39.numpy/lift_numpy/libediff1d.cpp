
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void ediff1d(float * v_initial_param_320_167, float * & v_user_func_323_168, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_323_168 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_165 = 0;(v_i_165 <= (-2 + v_N_0)); (++v_i_165)){
        // For each element reduced sequentially
        v_user_func_323_168[v_i_165] = 0.0f; 
        for (int v_i_166 = 0;(v_i_166 <= 1); (++v_i_166)){
            v_user_func_323_168[v_i_165] = diff2(v_user_func_323_168[v_i_165], v_initial_param_320_167[(v_i_165 + v_i_166)]); 
        }
    }
}
}; 