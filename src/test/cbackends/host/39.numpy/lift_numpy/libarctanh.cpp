
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
float arctanh_uf(float x){
    { return atanh(x); }; 
}

#endif
 ; 
void arctanh(float * v_initial_param_7189_2908, float * & v_user_func_7191_2909, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7191_2909 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2907 = 0;(v_i_2907 <= (-1 + v_N_2763)); (++v_i_2907)){
        v_user_func_7191_2909[v_i_2907] = arctanh_uf(v_initial_param_7189_2908[v_i_2907]); 
    }
}
}; 