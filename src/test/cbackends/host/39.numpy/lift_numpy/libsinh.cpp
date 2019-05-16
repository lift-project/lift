
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
void sinh(float * v_initial_param_185_95, float * & v_user_func_187_96, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_187_96 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_94 = 0;(v_i_94 <= (-1 + v_N_0)); (++v_i_94)){
        v_user_func_187_96[v_i_94] = sinh_uf(v_initial_param_185_95[v_i_94]); 
    }
}
}; 