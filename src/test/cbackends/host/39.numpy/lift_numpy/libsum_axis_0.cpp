
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
void sum_axis_0(float * v_initial_param_322_178, float * & v_user_func_325_179, int v_N_0, int v_M_1){
    // Allocate memory for output pointers
    v_user_func_325_179 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_176 = 0;(v_i_176 <= (-1 + v_N_0)); (++v_i_176)){
        // For each element reduced sequentially
        v_user_func_325_179[v_i_176] = 0.0f; 
        for (int v_i_177 = 0;(v_i_177 <= (-1 + v_M_1)); (++v_i_177)){
            v_user_func_325_179[v_i_176] = add(v_user_func_325_179[v_i_176], v_initial_param_322_178[(v_i_176 + (v_N_0 * v_i_177))]); 
        }
    }
}
}; 