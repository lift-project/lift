
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINH_UF_H
#define SINH_UF_H
; 
float sinh_uf(float x){
    { return sinh(x); }; 
}

#endif
 ; 
void sinh(float * v_initial_param_7154_2893, float * & v_user_func_7156_2894, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7156_2894 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2892 = 0;(v_i_2892 <= (-1 + v_N_2763)); (++v_i_2892)){
        v_user_func_7156_2894[v_i_2892] = sinh_uf(v_initial_param_7154_2893[v_i_2892]); 
    }
}
}; 