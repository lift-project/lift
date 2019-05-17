
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
void floor_divide(float * v_initial_param_674_286, float * v_initial_param_675_287, float * & v_user_func_681_289, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_681_289 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_285 = 0;(v_i_285 <= (-1 + v_N_0)); (++v_i_285)){
        v_user_func_681_289[v_i_285] = floor_div_uf(v_initial_param_674_286[v_i_285], v_initial_param_675_287[v_i_285]); 
    }
}
}; 