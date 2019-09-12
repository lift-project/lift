
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
void nansum(float * v_initial_param_7250_2957, float * & v_user_func_7253_2958, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7253_2958 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_7253_2958[0] = 0.0f; 
    for (int v_i_2956 = 0;(v_i_2956 <= (-1 + v_N_2763)); (++v_i_2956)){
        v_user_func_7253_2958[0] = add(v_user_func_7253_2958[0], v_initial_param_7250_2957[v_i_2956]); 
    }
}
}; 