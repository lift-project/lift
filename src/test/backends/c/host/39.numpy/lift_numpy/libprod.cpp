
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef PROD2_UF_H
#define PROD2_UF_H
; 
float prod2_uf(float l, float r){
    { return (l * r); }; 
}

#endif
 ; 
void prod(float * v_initial_param_7238_2932, float * & v_user_func_7241_2933, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7241_2933 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_7241_2933[0] = 1.0f; 
    for (int v_i_2931 = 0;(v_i_2931 <= (-1 + v_N_2763)); (++v_i_2931)){
        v_user_func_7241_2933[0] = prod2_uf(v_user_func_7241_2933[0], v_initial_param_7238_2932[v_i_2931]); 
    }
}
}; 