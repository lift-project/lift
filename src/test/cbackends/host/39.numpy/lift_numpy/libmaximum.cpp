
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MAXIMUM_UF_H
#define MAXIMUM_UF_H
; 
float maximum_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void maximum(float * v_initial_param_895_394, float * v_initial_param_896_395, float * & v_user_func_902_397, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_902_397 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_393 = 0;(v_i_393 <= (-1 + v_N_0)); (++v_i_393)){
        v_user_func_902_397[v_i_393] = maximum_uf(v_initial_param_895_394[v_i_393], v_initial_param_896_395[v_i_393]); 
    }
}
}; 