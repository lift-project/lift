
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
void arctanh(float * v_initial_param_231_123, float * & v_user_func_233_124, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_233_124 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_122 = 0;(v_i_122 <= (-1 + v_N_0)); (++v_i_122)){
        v_user_func_233_124[v_i_122] = arctanh_uf(v_initial_param_231_123[v_i_122]); 
    }
}
}; 