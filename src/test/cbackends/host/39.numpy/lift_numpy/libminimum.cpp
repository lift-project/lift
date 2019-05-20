
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MINIMUM_UF_H
#define MINIMUM_UF_H
; 
float minimum_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void minimum(float * v_initial_param_909_399, float * v_initial_param_910_400, float * & v_user_func_916_402, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_916_402 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_398 = 0;(v_i_398 <= (-1 + v_N_0)); (++v_i_398)){
        v_user_func_916_402[v_i_398] = minimum_uf(v_initial_param_909_399[v_i_398], v_initial_param_910_400[v_i_398]); 
    }
}
}; 