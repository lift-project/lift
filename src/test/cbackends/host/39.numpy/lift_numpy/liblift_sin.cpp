
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIN_UF_H
#define SIN_UF_H
; 
float sin_uf(float x){
    { return sin(x); }; 
}

#endif
 ; 
void lift_sin(float * v_initial_param_96_58, float * & v_user_func_98_59, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_98_59 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_57 = 0;(v_i_57 <= (-1 + v_N_0)); (++v_i_57)){
        v_user_func_98_59[v_i_57] = sin_uf(v_initial_param_96_58[v_i_57]); 
    }
}
}; 