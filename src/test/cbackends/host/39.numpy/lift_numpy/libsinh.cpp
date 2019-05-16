
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
void sinh(float * v_initial_param_175_85, float * & v_user_func_177_86, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_177_86 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_84 = 0;(v_i_84 <= (-1 + v_N_0)); (++v_i_84)){
        v_user_func_177_86[v_i_84] = sinh_uf(v_initial_param_175_85[v_i_84]); 
    }
}
}; 