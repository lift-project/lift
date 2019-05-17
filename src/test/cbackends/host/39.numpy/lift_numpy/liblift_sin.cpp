
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
void lift_sin(float * v_initial_param_102_64, float * & v_user_func_104_65, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_104_65 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_63 = 0;(v_i_63 <= (-1 + v_N_0)); (++v_i_63)){
        v_user_func_104_65[v_i_63] = sin_uf(v_initial_param_102_64[v_i_63]); 
    }
}
}; 