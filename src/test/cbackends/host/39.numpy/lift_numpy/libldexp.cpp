
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
void ldexp(float * v_initial_param_608_273, float * v_initial_param_609_274, float * & v_user_func_615_276, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_615_276 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_272 = 0;(v_i_272 <= (-1 + v_N_0)); (++v_i_272)){
        v_user_func_615_276[v_i_272] = ldexp_uf(v_initial_param_608_273[v_i_272], v_initial_param_609_274[v_i_272]); 
    }
}
}; 