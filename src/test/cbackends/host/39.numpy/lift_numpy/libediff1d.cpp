
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
void ediff1d(float * v_initial_param_335_183, float * & v_user_func_338_184, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_338_184 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_181 = 0;(v_i_181 <= (-2 + v_N_0)); (++v_i_181)){
        // For each element reduced sequentially
        v_user_func_338_184[v_i_181] = 0.0f; 
        for (int v_i_182 = 0;(v_i_182 <= 1); (++v_i_182)){
            v_user_func_338_184[v_i_181] = diff2(v_user_func_338_184[v_i_181], v_initial_param_335_183[(v_i_181 + v_i_182)]); 
        }
    }
}
}; 