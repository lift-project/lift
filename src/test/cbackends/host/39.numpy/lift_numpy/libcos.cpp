
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef COS_UF_H
#define COS_UF_H
; 
float cos_uf(float x){
    { return cos(x); }; 
}

#endif; 
void cos(float * v_initial_param_85_34, float * & v_user_func_87_35, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_87_35 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_33 = 0;(v_i_33 <= (-1 + v_N_0)); (++v_i_33)){
        v_user_func_87_35[v_i_33] = cos_uf(v_initial_param_85_34[v_i_33]); 
    }
}
}; 