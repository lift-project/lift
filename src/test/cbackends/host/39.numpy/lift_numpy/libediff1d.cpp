
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
void ediff1d(float * v_initial_param_365_210, float * & v_user_func_368_211, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_368_211 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_208 = 0;(v_i_208 <= (-2 + v_N_0)); (++v_i_208)){
        // For each element reduced sequentially
        v_user_func_368_211[v_i_208] = 0.0f; 
        for (int v_i_209 = 0;(v_i_209 <= 1); (++v_i_209)){
            v_user_func_368_211[v_i_208] = diff2(v_user_func_368_211[v_i_208], v_initial_param_365_210[(v_i_208 + v_i_209)]); 
        }
    }
}
}; 