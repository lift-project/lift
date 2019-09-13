
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MULTANDSUMUP_H
#define MULTANDSUMUP_H
; 
float multAndSumUp(float acc, float l, float r){
    { return acc + (l * r); }; 
}

#endif
 ; 
void convolve(float * v_initial_param_7784_3138, float * v_initial_param_7785_3139, float * & v_user_func_7793_3141, int v_M_2764, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7793_3141 = reinterpret_cast<float *>(malloc(((1 + v_M_2764 + (-1 * v_N_2763)) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3136 = 0;(v_i_3136 <= (v_M_2764 + (-1 * v_N_2763))); (++v_i_3136)){
        {
            
        }
        {
            
        }
        // For each element reduced sequentially
        v_user_func_7793_3141[v_i_3136] = 0.0f; 
        for (int v_i_3137 = 0;(v_i_3137 <= (-1 + v_N_2763)); (++v_i_3137)){
            v_user_func_7793_3141[v_i_3136] = multAndSumUp(v_user_func_7793_3141[v_i_3136], v_initial_param_7784_3138[(v_i_3136 + v_i_3137)], v_initial_param_7785_3139[v_i_3137]); 
        }
    }
}
}; 