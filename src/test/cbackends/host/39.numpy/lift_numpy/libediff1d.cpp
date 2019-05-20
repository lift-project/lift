
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
void ediff1d(float * v_initial_param_378_216, float * & v_user_func_381_217, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_381_217 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_214 = 0;(v_i_214 <= (-2 + v_N_0)); (++v_i_214)){
        // For each element reduced sequentially
        v_user_func_381_217[v_i_214] = 0.0f; 
        for (int v_i_215 = 0;(v_i_215 <= 1); (++v_i_215)){
            v_user_func_381_217[v_i_214] = diff2(v_user_func_381_217[v_i_214], v_initial_param_378_216[(v_i_214 + v_i_215)]); 
        }
    }
}
}; 