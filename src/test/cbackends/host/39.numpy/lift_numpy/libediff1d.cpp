
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
void ediff1d(float * v_initial_param_314_160, float * & v_user_func_317_161, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_317_161 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_158 = 0;(v_i_158 <= (-2 + v_N_0)); (++v_i_158)){
        // For each element reduced sequentially
        v_user_func_317_161[v_i_158] = 0.0f; 
        for (int v_i_159 = 0;(v_i_159 <= 1); (++v_i_159)){
            v_user_func_317_161[v_i_158] = diff2(v_user_func_317_161[v_i_158], v_initial_param_314_160[(v_i_158 + v_i_159)]); 
        }
    }
}
}; 