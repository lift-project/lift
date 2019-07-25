
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef POWER_UF_H
#define POWER_UF_H
; 
float power_uf(float x, float y){
    return pow(x, y);; 
}

#endif
 ; 
void float_power(float * v_initial_param_3694_683, float * v_initial_param_3695_684, float * & v_user_func_3701_686, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3701_686 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_682 = 0;(v_i_682 <= (-1 + v_N_352)); (++v_i_682)){
        v_user_func_3701_686[v_i_682] = power_uf(v_initial_param_3694_683[v_i_682], v_initial_param_3695_684[v_i_682]); 
    }
}
}; 