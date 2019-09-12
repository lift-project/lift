
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
void fabs(float * v_initial_param_7834_3155, float * & v_user_func_7836_3156, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7836_3156 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3154 = 0;(v_i_3154 <= (-1 + v_N_2763)); (++v_i_3154)){
        v_user_func_7836_3156[v_i_3154] = absolute_uf(v_initial_param_7834_3155[v_i_3154]); 
    }
}
}; 