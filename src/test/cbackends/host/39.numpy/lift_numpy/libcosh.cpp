
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
void cosh(float * v_initial_param_194_100, float * & v_user_func_196_101, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_196_101 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_99 = 0;(v_i_99 <= (-1 + v_N_0)); (++v_i_99)){
        v_user_func_196_101[v_i_99] = cosh_uf(v_initial_param_194_100[v_i_99]); 
    }
}
}; 