
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
void ediff1d(float * v_initial_param_311_157, float * & v_user_func_314_158, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_314_158 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_155 = 0;(v_i_155 <= (-2 + v_N_0)); (++v_i_155)){
        // For each element reduced sequentially
        v_user_func_314_158[v_i_155] = 0.0f; 
        for (int v_i_156 = 0;(v_i_156 <= 1); (++v_i_156)){
            v_user_func_314_158[v_i_155] = diff2(v_user_func_314_158[v_i_155], v_initial_param_311_157[(v_i_155 + v_i_156)]); 
        }
    }
}
}; 