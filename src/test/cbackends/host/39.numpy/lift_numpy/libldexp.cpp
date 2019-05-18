
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
void ldexp(float * v_initial_param_575_243, float * v_initial_param_576_244, float * & v_user_func_582_246, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_582_246 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_242 = 0;(v_i_242 <= (-1 + v_N_0)); (++v_i_242)){
        v_user_func_582_246[v_i_242] = ldexp_uf(v_initial_param_575_243[v_i_242], v_initial_param_576_244[v_i_242]); 
    }
}
}; 