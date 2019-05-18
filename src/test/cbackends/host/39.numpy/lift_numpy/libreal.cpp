
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
void real(Tuple2_float_float * v_initial_param_768_334, float * & v_user_func_774_335, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_774_335 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_333 = 0;(v_i_333 <= (-1 + v_N_0)); (++v_i_333)){
        v_user_func_774_335[v_i_333] = real_uf(v_initial_param_768_334[v_i_333]._0, v_initial_param_768_334[v_i_333]._1); 
    }
}
}; 