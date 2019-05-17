
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
void cosh(float * v_initial_param_202_110, float * & v_user_func_204_111, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_204_111 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_109 = 0;(v_i_109 <= (-1 + v_N_0)); (++v_i_109)){
        v_user_func_204_111[v_i_109] = cosh_uf(v_initial_param_202_110[v_i_109]); 
    }
}
}; 