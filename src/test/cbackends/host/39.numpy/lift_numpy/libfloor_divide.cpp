
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
void floor_divide(float * v_initial_param_676_288, float * v_initial_param_677_289, float * & v_user_func_683_291, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_683_291 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_287 = 0;(v_i_287 <= (-1 + v_N_0)); (++v_i_287)){
        v_user_func_683_291[v_i_287] = floor_div_uf(v_initial_param_676_288[v_i_287], v_initial_param_677_289[v_i_287]); 
    }
}
}; 