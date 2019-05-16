
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
void cos(float * v_initial_param_88_45, float * & v_user_func_90_46, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_90_46 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_44 = 0;(v_i_44 <= (-1 + v_N_0)); (++v_i_44)){
        v_user_func_90_46[v_i_44] = cos_uf(v_initial_param_88_45[v_i_44]); 
    }
}
}; 