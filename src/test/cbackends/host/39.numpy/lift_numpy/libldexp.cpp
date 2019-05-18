
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
void ldexp(float * v_initial_param_578_248, float * v_initial_param_579_249, float * & v_user_func_585_251, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_585_251 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_247 = 0;(v_i_247 <= (-1 + v_N_0)); (++v_i_247)){
        v_user_func_585_251[v_i_247] = ldexp_uf(v_initial_param_578_248[v_i_247], v_initial_param_579_249[v_i_247]); 
    }
}
}; 