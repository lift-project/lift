
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RINT_UF_H
#define RINT_UF_H
; 
float rint_uf(float x){
    return round(x) ;; 
}

#endif
 ; 
void rint(float * v_initial_param_224_109, float * & v_user_func_226_110, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_226_110 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_108 = 0;(v_i_108 <= (-1 + v_N_0)); (++v_i_108)){
        v_user_func_226_110[v_i_108] = rint_uf(v_initial_param_224_109[v_i_108]); 
    }
}
}; 