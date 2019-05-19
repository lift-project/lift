
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef REAL_UF_H
#define REAL_UF_H
; 
float real_uf(float x, float y){
    { return x; }; 
}

#endif
 ; 
void real(Tuple2_float_float * v_initial_param_779_345, float * & v_user_func_785_346, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_785_346 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_344 = 0;(v_i_344 <= (-1 + v_N_0)); (++v_i_344)){
        v_user_func_785_346[v_i_344] = real_uf(v_initial_param_779_345[v_i_344]._0, v_initial_param_779_345[v_i_344]._1); 
    }
}
}; 