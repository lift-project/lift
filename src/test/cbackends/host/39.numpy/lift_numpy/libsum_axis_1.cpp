
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void sum_axis_1(float * v_initial_param_341_190, float * & v_user_func_344_191, int v_N_0, int v_M_1){
    // Allocate memory for output pointers
    v_user_func_344_191 = reinterpret_cast<float *>(malloc((v_M_1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_188 = 0;(v_i_188 <= (-1 + v_M_1)); (++v_i_188)){
        // For each element reduced sequentially
        v_user_func_344_191[v_i_188] = 0.0f; 
        for (int v_i_189 = 0;(v_i_189 <= (-1 + v_N_0)); (++v_i_189)){
            v_user_func_344_191[v_i_188] = add(v_user_func_344_191[v_i_188], v_initial_param_341_190[(v_i_189 + (v_N_0 * v_i_188))]); 
        }
    }
}
}; 