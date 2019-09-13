
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SQRT_UF_H
#define SQRT_UF_H
; 
float sqrt_uf(float x){
    { return sqrt(x); }; 
}

#endif
 ; 
void sqrt(float * v_initial_param_7813_3143, float * & v_user_func_7815_3144, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7815_3144 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3142 = 0;(v_i_3142 <= (-1 + v_N_2763)); (++v_i_3142)){
        v_user_func_7815_3144[v_i_3142] = sqrt_uf(v_initial_param_7813_3143[v_i_3142]); 
    }
}
}; 