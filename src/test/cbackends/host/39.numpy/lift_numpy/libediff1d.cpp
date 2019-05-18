
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
void ediff1d(float * v_initial_param_335_185, float * & v_user_func_338_186, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_338_186 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_183 = 0;(v_i_183 <= (-2 + v_N_0)); (++v_i_183)){
        // For each element reduced sequentially
        v_user_func_338_186[v_i_183] = 0.0f; 
        for (int v_i_184 = 0;(v_i_184 <= 1); (++v_i_184)){
            v_user_func_338_186[v_i_183] = diff2(v_user_func_338_186[v_i_183], v_initial_param_335_185[(v_i_183 + v_i_184)]); 
        }
    }
}
}; 