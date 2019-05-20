
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIGN_UF_H
#define SIGN_UF_H
; 
float sign_uf(float x){
    { return x>=0?1:-1; }; 
}

#endif
 ; 
void sign(float * v_initial_param_888_391, float * & v_user_func_890_392, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_890_392 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_390 = 0;(v_i_390 <= (-1 + v_N_0)); (++v_i_390)){
        v_user_func_890_392[v_i_390] = sign_uf(v_initial_param_888_391[v_i_390]); 
    }
}
}; 