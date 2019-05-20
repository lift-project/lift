
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COS_UF_H
#define COS_UF_H
; 
float cos_uf(float x){
    { return cos(x); }; 
}

#endif
 ; 
void cos(float * v_initial_param_128_91, float * & v_user_func_130_92, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_130_92 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_90 = 0;(v_i_90 <= (-1 + v_N_0)); (++v_i_90)){
        v_user_func_130_92[v_i_90] = cos_uf(v_initial_param_128_91[v_i_90]); 
    }
}
}; 