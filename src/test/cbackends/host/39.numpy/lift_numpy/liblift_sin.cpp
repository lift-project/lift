
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
void lift_sin(float * v_initial_param_7061_2851, float * & v_user_func_7063_2852, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7063_2852 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2850 = 0;(v_i_2850 <= (-1 + v_N_2763)); (++v_i_2850)){
        v_user_func_7063_2852[v_i_2850] = sin_uf(v_initial_param_7061_2851[v_i_2850]); 
    }
}
}; 