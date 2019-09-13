
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ABSOLUTE_UF_H
#define ABSOLUTE_UF_H
; 
float absolute_uf(float x){
    { return abs(x); }; 
}

#endif
 ; 
void absolute(float * v_initial_param_7834_3152, float * & v_user_func_7836_3153, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7836_3153 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3151 = 0;(v_i_3151 <= (-1 + v_N_2763)); (++v_i_3151)){
        v_user_func_7836_3153[v_i_3151] = absolute_uf(v_initial_param_7834_3152[v_i_3151]); 
    }
}
}; 