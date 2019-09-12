
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_DIV_UF_H
#define FLOOR_DIV_UF_H
; 
float floor_div_uf(float x, float y){
    return floor(x/y);; 
}

#endif
 ; 
void floor_divide(float * v_initial_param_7666_3089, float * v_initial_param_7667_3090, float * & v_user_func_7673_3092, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7673_3092 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3088 = 0;(v_i_3088 <= (-1 + v_N_2763)); (++v_i_3088)){
        v_user_func_7673_3092[v_i_3088] = floor_div_uf(v_initial_param_7666_3089[v_i_3088], v_initial_param_7667_3090[v_i_3088]); 
    }
}
}; 