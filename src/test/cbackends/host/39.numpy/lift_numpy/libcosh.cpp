
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COSH_UF_H
#define COSH_UF_H
; 
float cosh_uf(float x){
    { return cosh(x); }; 
}

#endif
 ; 
void cosh(float * v_initial_param_187_93, float * & v_user_func_189_94, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_189_94 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_92 = 0;(v_i_92 <= (-1 + v_N_0)); (++v_i_92)){
        v_user_func_189_94[v_i_92] = cosh_uf(v_initial_param_187_93[v_i_92]); 
    }
}
}; 