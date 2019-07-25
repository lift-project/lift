
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
void tanh(float * v_initial_param_3224_488, float * & v_user_func_3226_489, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3226_489 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_487 = 0;(v_i_487 <= (-1 + v_N_352)); (++v_i_487)){
        v_user_func_3226_489[v_i_487] = tanh_uf(v_initial_param_3224_488[v_i_487]); 
    }
}
}; 