
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
void lift_sin(float * v_initial_param_103_65, float * & v_user_func_105_66, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_105_66 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_64 = 0;(v_i_64 <= (-1 + v_N_0)); (++v_i_64)){
        v_user_func_105_66[v_i_64] = sin_uf(v_initial_param_103_65[v_i_64]); 
    }
}
}; 