
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINH_UF_H
#define SINH_UF_H
; 
float sinh_uf(float x){
    { return sinh(x); }; 
}

#endif
 ; 
void sinh(float * v_initial_param_176_86, float * & v_user_func_178_87, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_178_87 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_85 = 0;(v_i_85 <= (-1 + v_N_0)); (++v_i_85)){
        v_user_func_178_87[v_i_85] = sinh_uf(v_initial_param_176_86[v_i_85]); 
    }
}
}; 