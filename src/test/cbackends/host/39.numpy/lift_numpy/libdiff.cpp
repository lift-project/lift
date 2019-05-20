
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
void diff(float * v_initial_param_365_206, float * & v_user_func_368_207, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_368_207 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_204 = 0;(v_i_204 <= (-2 + v_N_0)); (++v_i_204)){
        // For each element reduced sequentially
        v_user_func_368_207[v_i_204] = 0.0f; 
        for (int v_i_205 = 0;(v_i_205 <= 1); (++v_i_205)){
            v_user_func_368_207[v_i_204] = diff2(v_user_func_368_207[v_i_204], v_initial_param_365_206[(v_i_204 + v_i_205)]); 
        }
    }
}
}; 