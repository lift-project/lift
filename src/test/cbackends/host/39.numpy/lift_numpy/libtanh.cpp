
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
void tanh(float * v_initial_param_1475_276, float * & v_user_func_1477_277, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1477_277 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_275 = 0;(v_i_275 <= (-1 + v_N_190)); (++v_i_275)){
        v_user_func_1477_277[v_i_275] = tanh_uf(v_initial_param_1475_276[v_i_275]); 
    }
}
}; 