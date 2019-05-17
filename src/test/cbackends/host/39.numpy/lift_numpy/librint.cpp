
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
void rint(float * v_initial_param_245_132, float * & v_user_func_247_133, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_247_133 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_131 = 0;(v_i_131 <= (-1 + v_N_0)); (++v_i_131)){
        v_user_func_247_133[v_i_131] = rint_uf(v_initial_param_245_132[v_i_131]); 
    }
}
}; 