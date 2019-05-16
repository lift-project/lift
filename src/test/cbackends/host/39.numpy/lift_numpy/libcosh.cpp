
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
void cosh(float * v_initial_param_189_95, float * & v_user_func_191_96, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_191_96 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_94 = 0;(v_i_94 <= (-1 + v_N_0)); (++v_i_94)){
        v_user_func_191_96[v_i_94] = cosh_uf(v_initial_param_189_95[v_i_94]); 
    }
}
}; 