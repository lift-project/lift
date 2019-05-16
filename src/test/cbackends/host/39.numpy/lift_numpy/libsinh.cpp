
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
void sinh(float * v_initial_param_183_93, float * & v_user_func_185_94, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_185_94 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_92 = 0;(v_i_92 <= (-1 + v_N_0)); (++v_i_92)){
        v_user_func_185_94[v_i_92] = sinh_uf(v_initial_param_183_93[v_i_92]); 
    }
}
}; 