
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
void diff(float * v_initial_param_7318_2973, float * & v_user_func_7321_2974, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7321_2974 = reinterpret_cast<float *>(malloc(((-1 + v_N_2763) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_2971 = 0;(v_i_2971 <= (-2 + v_N_2763)); (++v_i_2971)){
        // For each element reduced sequentially
        v_user_func_7321_2974[v_i_2971] = 0.0f; 
        for (int v_i_2972 = 0;(v_i_2972 <= 1); (++v_i_2972)){
            v_user_func_7321_2974[v_i_2971] = diff2(v_user_func_7321_2974[v_i_2971], v_initial_param_7318_2973[(v_i_2971 + v_i_2972)]); 
        }
    }
}
}; 