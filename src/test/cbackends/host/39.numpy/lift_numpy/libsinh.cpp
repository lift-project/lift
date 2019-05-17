
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
void sinh(float * v_initial_param_196_108, float * & v_user_func_198_109, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_198_109 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_107 = 0;(v_i_107 <= (-1 + v_N_0)); (++v_i_107)){
        v_user_func_198_109[v_i_107] = sinh_uf(v_initial_param_196_108[v_i_107]); 
    }
}
}; 