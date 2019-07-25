
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
void power(float * v_initial_param_3694_663, float * v_initial_param_3695_664, float * & v_user_func_3701_666, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3701_666 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_662 = 0;(v_i_662 <= (-1 + v_N_352)); (++v_i_662)){
        v_user_func_3701_666[v_i_662] = power_uf(v_initial_param_3694_663[v_i_662], v_initial_param_3695_664[v_i_662]); 
    }
}
}; 