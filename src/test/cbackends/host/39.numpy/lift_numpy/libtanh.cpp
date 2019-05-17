
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
void tanh(float * v_initial_param_206_110, float * & v_user_func_208_111, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_208_111 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_109 = 0;(v_i_109 <= (-1 + v_N_0)); (++v_i_109)){
        v_user_func_208_111[v_i_109] = tanh_uf(v_initial_param_206_110[v_i_109]); 
    }
}
}; 