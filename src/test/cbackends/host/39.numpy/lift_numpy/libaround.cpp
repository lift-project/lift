
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ROUND_UF_H
#define ROUND_UF_H
; 
float round_uf(float x){
    return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;; 
}

#endif
 ; 
void around(float * v_initial_param_7196_2911, float * & v_user_func_7198_2912, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7198_2912 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2910 = 0;(v_i_2910 <= (-1 + v_N_2763)); (++v_i_2910)){
        v_user_func_7198_2912[v_i_2910] = round_uf(v_initial_param_7196_2911[v_i_2910]); 
    }
}
}; 