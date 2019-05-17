
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
void cosh(float * v_initial_param_200_108, float * & v_user_func_202_109, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_202_109 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_107 = 0;(v_i_107 <= (-1 + v_N_0)); (++v_i_107)){
        v_user_func_202_109[v_i_107] = cosh_uf(v_initial_param_200_108[v_i_107]); 
    }
}
}; 