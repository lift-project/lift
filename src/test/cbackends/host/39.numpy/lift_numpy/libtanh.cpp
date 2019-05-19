
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
void tanh(float * v_initial_param_228_132, float * & v_user_func_230_133, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_230_133 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_131 = 0;(v_i_131 <= (-1 + v_N_0)); (++v_i_131)){
        v_user_func_230_133[v_i_131] = tanh_uf(v_initial_param_228_132[v_i_131]); 
    }
}
}; 