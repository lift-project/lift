
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
void rint(float * v_initial_param_230_115, float * & v_user_func_232_116, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_232_116 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_114 = 0;(v_i_114 <= (-1 + v_N_0)); (++v_i_114)){
        v_user_func_232_116[v_i_114] = rint_uf(v_initial_param_230_115[v_i_114]); 
    }
}
}; 