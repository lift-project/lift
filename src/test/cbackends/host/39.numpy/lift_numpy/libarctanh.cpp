
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
float arctanh_uf(float x){
    { return atanh(x); }; 
}

#endif
 ; 
void arctanh(float * v_initial_param_229_121, float * & v_user_func_231_122, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_231_122 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_120 = 0;(v_i_120 <= (-1 + v_N_0)); (++v_i_120)){
        v_user_func_231_122[v_i_120] = arctanh_uf(v_initial_param_229_121[v_i_120]); 
    }
}
}; 