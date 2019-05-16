
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
void rint(float * v_initial_param_222_107, float * & v_user_func_224_108, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_224_108 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_106 = 0;(v_i_106 <= (-1 + v_N_0)); (++v_i_106)){
        v_user_func_224_108[v_i_106] = rint_uf(v_initial_param_222_107[v_i_106]); 
    }
}
}; 