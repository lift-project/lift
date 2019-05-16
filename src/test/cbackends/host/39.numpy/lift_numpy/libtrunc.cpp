
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
float trunc_uf(float x){
    return trunc(x);; 
}

#endif
 ; 
void trunc(float * v_initial_param_258_127, float * & v_user_func_260_128, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_260_128 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_126 = 0;(v_i_126 <= (-1 + v_N_0)); (++v_i_126)){
        v_user_func_260_128[v_i_126] = trunc_uf(v_initial_param_258_127[v_i_126]); 
    }
}
}; 