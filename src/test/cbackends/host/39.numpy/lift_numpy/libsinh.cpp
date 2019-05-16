
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
void sinh(float * v_initial_param_173_83, float * & v_user_func_175_84, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_175_84 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_82 = 0;(v_i_82 <= (-1 + v_N_0)); (++v_i_82)){
        v_user_func_175_84[v_i_82] = sinh_uf(v_initial_param_173_83[v_i_82]); 
    }
}
}; 