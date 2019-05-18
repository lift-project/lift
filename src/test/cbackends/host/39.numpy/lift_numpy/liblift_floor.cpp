
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
void lift_floor(float * v_initial_param_266_145, float * & v_user_func_268_146, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_268_146 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_144 = 0;(v_i_144 <= (-1 + v_N_0)); (++v_i_144)){
        v_user_func_268_146[v_i_144] = floor_uf(v_initial_param_266_145[v_i_144]); 
    }
}
}; 