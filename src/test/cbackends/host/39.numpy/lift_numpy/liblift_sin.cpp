
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
void lift_sin(float * v_initial_param_97_59, float * & v_user_func_99_60, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_99_60 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_58 = 0;(v_i_58 <= (-1 + v_N_0)); (++v_i_58)){
        v_user_func_99_60[v_i_58] = sin_uf(v_initial_param_97_59[v_i_58]); 
    }
}
}; 