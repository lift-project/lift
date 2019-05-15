
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
void cos(float * v_initial_param_85_39, float * & v_user_func_87_40, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_87_40 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_38 = 0;(v_i_38 <= (-1 + v_N_0)); (++v_i_38)){
        v_user_func_87_40[v_i_38] = cos_uf(v_initial_param_85_39[v_i_38]); 
    }
}
}; 