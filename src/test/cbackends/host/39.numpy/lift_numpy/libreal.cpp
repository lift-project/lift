
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
void real(Tuple2_float_float * v_initial_param_768_336, float * & v_user_func_774_337, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_774_337 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_335 = 0;(v_i_335 <= (-1 + v_N_0)); (++v_i_335)){
        v_user_func_774_337[v_i_335] = real_uf(v_initial_param_768_336[v_i_335]._0, v_initial_param_768_336[v_i_335]._1); 
    }
}
}; 