
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
void ldexp(float * v_initial_param_567_235, float * v_initial_param_568_236, float * & v_user_func_574_238, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_574_238 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_234 = 0;(v_i_234 <= (-1 + v_N_0)); (++v_i_234)){
        v_user_func_574_238[v_i_234] = ldexp_uf(v_initial_param_567_235[v_i_234], v_initial_param_568_236[v_i_234]); 
    }
}
}; 