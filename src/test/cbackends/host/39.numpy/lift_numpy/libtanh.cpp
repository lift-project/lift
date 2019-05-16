
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
void tanh(float * v_initial_param_201_104, float * & v_user_func_203_105, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_203_105 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_103 = 0;(v_i_103 <= (-1 + v_N_0)); (++v_i_103)){
        v_user_func_203_105[v_i_103] = tanh_uf(v_initial_param_201_104[v_i_103]); 
    }
}
}; 