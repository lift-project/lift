
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef PROD2_UF_H
#define PROD2_UF_H
; 
float prod2_uf(float l, float r){
    { return (l * r); }; 
}

#endif
 ; 
void prod(float * v_initial_param_3294_521, float * & v_user_func_3297_522, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3297_522 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_3297_522[0] = 1.0f; 
    for (int v_i_520 = 0;(v_i_520 <= (-1 + v_N_352)); (++v_i_520)){
        v_user_func_3297_522[0] = prod2_uf(v_user_func_3297_522[0], v_initial_param_3294_521[v_i_520]); 
    }
}
}; 