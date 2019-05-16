
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TANH_UF_H
#define TANH_UF_H
; 
float tanh_uf(float x){
    { return tanh(x); }; 
}

#endif
 ; 
void tanh(float * v_initial_param_195_97, float * & v_user_func_197_98, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_197_98 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_96 = 0;(v_i_96 <= (-1 + v_N_0)); (++v_i_96)){
        v_user_func_197_98[v_i_96] = tanh_uf(v_initial_param_195_97[v_i_96]); 
    }
}
}; 