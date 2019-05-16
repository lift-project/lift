
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
void ediff1d(float * v_initial_param_309_155, float * & v_user_func_312_156, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_312_156 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_153 = 0;(v_i_153 <= (-2 + v_N_0)); (++v_i_153)){
        // For each element reduced sequentially
        v_user_func_312_156[v_i_153] = 0.0f; 
        for (int v_i_154 = 0;(v_i_154 <= 1); (++v_i_154)){
            v_user_func_312_156[v_i_153] = diff2(v_user_func_312_156[v_i_153], v_initial_param_309_155[(v_i_153 + v_i_154)]); 
        }
    }
}
}; 