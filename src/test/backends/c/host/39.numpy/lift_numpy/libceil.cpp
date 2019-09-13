
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef CEIL_UF_H
#define CEIL_UF_H
; 
float ceil_uf(float x){
    return ceil(x);; 
}

#endif
 ; 
void ceil(float * v_initial_param_7224_2926, float * & v_user_func_7226_2927, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7226_2927 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2925 = 0;(v_i_2925 <= (-1 + v_N_2763)); (++v_i_2925)){
        v_user_func_7226_2927[v_i_2925] = ceil_uf(v_initial_param_7224_2926[v_i_2925]); 
    }
}
}; 