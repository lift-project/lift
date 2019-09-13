
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SUBTRACT_H
#define SUBTRACT_H
; 
float subtract(float l, float r){
    { return l - r; }; 
}

#endif
 ; 
void subtract(float * v_initial_param_7652_3079, float * v_initial_param_7653_3080, float * & v_user_func_7659_3082, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7659_3082 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3078 = 0;(v_i_3078 <= (-1 + v_N_2763)); (++v_i_3078)){
        v_user_func_7659_3082[v_i_3078] = subtract(v_initial_param_7652_3079[v_i_3078], v_initial_param_7653_3080[v_i_3078]); 
    }
}
}; 