
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TANH_UF_H
#define TANH_UF_H
; 
float tanh_uf(float x){
    { return tanh(x); }; 
}

#endif
 ; 
void tanh(float * v_initial_param_197_99, float * & v_user_func_199_100, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_199_100 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_98 = 0;(v_i_98 <= (-1 + v_N_0)); (++v_i_98)){
        v_user_func_199_100[v_i_98] = tanh_uf(v_initial_param_197_99[v_i_98]); 
    }
}
}; 