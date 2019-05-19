
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
void rint(float * v_initial_param_263_150, float * & v_user_func_265_151, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_265_151 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_149 = 0;(v_i_149 <= (-1 + v_N_0)); (++v_i_149)){
        v_user_func_265_151[v_i_149] = rint_uf(v_initial_param_263_150[v_i_149]); 
    }
}
}; 