
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
void rint(float * v_initial_param_236_121, float * & v_user_func_238_122, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_238_122 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_120 = 0;(v_i_120 <= (-1 + v_N_0)); (++v_i_120)){
        v_user_func_238_122[v_i_120] = rint_uf(v_initial_param_236_121[v_i_120]); 
    }
}
}; 