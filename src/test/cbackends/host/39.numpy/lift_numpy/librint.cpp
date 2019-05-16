
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
void rint(float * v_initial_param_229_114, float * & v_user_func_231_115, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_231_115 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_113 = 0;(v_i_113 <= (-1 + v_N_0)); (++v_i_113)){
        v_user_func_231_115[v_i_113] = rint_uf(v_initial_param_229_114[v_i_113]); 
    }
}
}; 