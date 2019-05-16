
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LDEXP_UF_H
#define LDEXP_UF_H
; 
float ldexp_uf(float x, float y){
    return x* pow(2,y) ;; 
}

#endif
 ; 
void ldexp(float * v_initial_param_562_229, float * v_initial_param_563_230, float * & v_user_func_569_232, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_569_232 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_228 = 0;(v_i_228 <= (-1 + v_N_0)); (++v_i_228)){
        v_user_func_569_232[v_i_228] = ldexp_uf(v_initial_param_562_229[v_i_228], v_initial_param_563_230[v_i_228]); 
    }
}
}; 