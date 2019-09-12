
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
void real(Tuple2_float_float * v_initial_param_7751_3128, float * & v_user_func_7757_3129, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7757_3129 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3127 = 0;(v_i_3127 <= (-1 + v_N_2763)); (++v_i_3127)){
        v_user_func_7757_3129[v_i_3127] = real_uf(v_initial_param_7751_3128[v_i_3127]._0, v_initial_param_7751_3128[v_i_3127]._1); 
    }
}
}; 