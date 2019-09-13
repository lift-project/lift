
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H
; 
float arccos_uf(float x){
    { return acos(x); }; 
}

#endif
 ; 
void arccosh(float * v_initial_param_7089_2905, float * & v_user_func_7091_2906, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7091_2906 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2904 = 0;(v_i_2904 <= (-1 + v_N_2763)); (++v_i_2904)){
        v_user_func_7091_2906[v_i_2904] = arccos_uf(v_initial_param_7089_2905[v_i_2904]); 
    }
}
}; 