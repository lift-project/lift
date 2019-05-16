
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
void cosh(float * v_initial_param_1468_273, float * & v_user_func_1470_274, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1470_274 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_272 = 0;(v_i_272 <= (-1 + v_N_190)); (++v_i_272)){
        v_user_func_1470_274[v_i_272] = cosh_uf(v_initial_param_1468_273[v_i_272]); 
    }
}
}; 