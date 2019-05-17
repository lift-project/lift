
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
float negative_uf(float x){
    return (-1.0f)*x; 
}

#endif
 ; 
void negative(float * v_initial_param_615_262, float * & v_user_func_617_263, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_617_263 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_261 = 0;(v_i_261 <= (-1 + v_N_0)); (++v_i_261)){
        v_user_func_617_263[v_i_261] = negative_uf(v_initial_param_615_262[v_i_261]); 
    }
}
}; 