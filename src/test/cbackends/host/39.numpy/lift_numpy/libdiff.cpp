
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
void diff(float * v_initial_param_346_192, float * & v_user_func_349_193, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_349_193 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_190 = 0;(v_i_190 <= (-2 + v_N_0)); (++v_i_190)){
        // For each element reduced sequentially
        v_user_func_349_193[v_i_190] = 0.0f; 
        for (int v_i_191 = 0;(v_i_191 <= 1); (++v_i_191)){
            v_user_func_349_193[v_i_190] = diff2(v_user_func_349_193[v_i_190], v_initial_param_346_192[(v_i_190 + v_i_191)]); 
        }
    }
}
}; 