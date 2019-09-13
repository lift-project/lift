
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
void cosh(float * v_initial_param_7161_2896, float * & v_user_func_7163_2897, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7163_2897 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2895 = 0;(v_i_2895 <= (-1 + v_N_2763)); (++v_i_2895)){
        v_user_func_7163_2897[v_i_2895] = cosh_uf(v_initial_param_7161_2896[v_i_2895]); 
    }
}
}; 