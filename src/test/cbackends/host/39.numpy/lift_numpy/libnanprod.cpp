
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
void nanprod(float * v_initial_param_7238_2954, float * & v_user_func_7241_2955, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7241_2955 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_7241_2955[0] = 1.0f; 
    for (int v_i_2953 = 0;(v_i_2953 <= (-1 + v_N_2763)); (++v_i_2953)){
        v_user_func_7241_2955[0] = prod2_uf(v_user_func_7241_2955[0], v_initial_param_7238_2954[v_i_2953]); 
    }
}
}; 