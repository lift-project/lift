
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
void sinh(float * v_initial_param_187_97, float * & v_user_func_189_98, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_189_98 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_96 = 0;(v_i_96 <= (-1 + v_N_0)); (++v_i_96)){
        v_user_func_189_98[v_i_96] = sinh_uf(v_initial_param_187_97[v_i_96]); 
    }
}
}; 