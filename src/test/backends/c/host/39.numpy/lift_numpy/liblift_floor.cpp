
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_UF_H
#define FLOOR_UF_H
; 
float floor_uf(float x){
    return floor(x);; 
}

#endif
 ; 
void lift_floor(float * v_initial_param_7217_2923, float * & v_user_func_7219_2924, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7219_2924 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2922 = 0;(v_i_2922 <= (-1 + v_N_2763)); (++v_i_2922)){
        v_user_func_7219_2924[v_i_2922] = floor_uf(v_initial_param_7217_2923[v_i_2922]); 
    }
}
}; 