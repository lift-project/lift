
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
void sin(float * v_initial_param_88_48, float * & v_user_func_90_49, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_90_49 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_47 = 0;(v_i_47 <= (-1 + v_N_0)); (++v_i_47)){
        v_user_func_90_49[v_i_47] = sin_uf(v_initial_param_88_48[v_i_47]); 
    }
}
}; 