
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
void lift_sin(float * v_initial_param_89_49, float * & v_user_func_91_50, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_91_50 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_48 = 0;(v_i_48 <= (-1 + v_N_0)); (++v_i_48)){
        v_user_func_91_50[v_i_48] = sin_uf(v_initial_param_89_49[v_i_48]); 
    }
}
}; 