
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
void minimum(float * v_initial_param_922_405, float * v_initial_param_923_406, float * & v_user_func_929_408, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_929_408 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_404 = 0;(v_i_404 <= (-1 + v_N_0)); (++v_i_404)){
        v_user_func_929_408[v_i_404] = minimum_uf(v_initial_param_922_405[v_i_404], v_initial_param_923_406[v_i_404]); 
    }
}
}; 