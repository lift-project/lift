
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
void tan(float * v_initial_param_118_73, float * & v_user_func_120_74, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_120_74 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_72 = 0;(v_i_72 <= (-1 + v_N_0)); (++v_i_72)){
        v_user_func_120_74[v_i_72] = tan_uf(v_initial_param_118_73[v_i_72]); 
    }
}
}; 