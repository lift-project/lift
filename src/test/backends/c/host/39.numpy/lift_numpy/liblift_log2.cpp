
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG2_UF_H
#define LOG2_UF_H
; 
float log2_uf(float x){
    return log2(x) ;; 
}

#endif
 ; 
void lift_log2(float * v_initial_param_7470_3010, float * & v_user_func_7472_3011, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7472_3011 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3009 = 0;(v_i_3009 <= (-1 + v_N_2763)); (++v_i_3009)){
        v_user_func_7472_3011[v_i_3009] = log2_uf(v_initial_param_7470_3010[v_i_3009]); 
    }
}
}; 