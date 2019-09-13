
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
void sum(float * v_initial_param_7250_2935, float * & v_user_func_7253_2936, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7253_2936 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_7253_2936[0] = 0.0f; 
    for (int v_i_2934 = 0;(v_i_2934 <= (-1 + v_N_2763)); (++v_i_2934)){
        v_user_func_7253_2936[0] = add(v_user_func_7253_2936[0], v_initial_param_7250_2935[v_i_2934]); 
    }
}
}; 