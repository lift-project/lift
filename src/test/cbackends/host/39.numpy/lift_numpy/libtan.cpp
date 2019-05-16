
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TAN_UF_H
#define TAN_UF_H
; 
float tan_uf(float x){
    { return tan(x); }; 
}

#endif
 ; 
void tan(float * v_initial_param_105_58, float * & v_user_func_107_59, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_107_59 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_57 = 0;(v_i_57 <= (-1 + v_N_0)); (++v_i_57)){
        v_user_func_107_59[v_i_57] = tan_uf(v_initial_param_105_58[v_i_57]); 
    }
}
}; 