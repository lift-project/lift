
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef CBRT_UF_H
#define CBRT_UF_H
; 
float cbrt_uf(float x){
    { return cbrt(x); }; 
}

#endif
 ; 
void cbrt(float * v_initial_param_7820_3146, float * & v_user_func_7822_3147, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7822_3147 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3145 = 0;(v_i_3145 <= (-1 + v_N_2763)); (++v_i_3145)){
        v_user_func_7822_3147[v_i_3145] = cbrt_uf(v_initial_param_7820_3146[v_i_3145]); 
    }
}
}; 