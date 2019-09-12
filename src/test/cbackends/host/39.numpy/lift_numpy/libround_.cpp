
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
void round_(float * v_initial_param_7196_2914, float * & v_user_func_7198_2915, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7198_2915 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2913 = 0;(v_i_2913 <= (-1 + v_N_2763)); (++v_i_2913)){
        v_user_func_7198_2915[v_i_2913] = round_uf(v_initial_param_7196_2914[v_i_2913]); 
    }
}
}; 