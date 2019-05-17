
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIN_UF_H
#define SIN_UF_H
; 
float sin_uf(float x){
    { return sin(x); }; 
}

#endif
 ; 
void lift_sin(float * v_initial_param_100_62, float * & v_user_func_102_63, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_102_63 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_61 = 0;(v_i_61 <= (-1 + v_N_0)); (++v_i_61)){
        v_user_func_102_63[v_i_61] = sin_uf(v_initial_param_100_62[v_i_61]); 
    }
}
}; 