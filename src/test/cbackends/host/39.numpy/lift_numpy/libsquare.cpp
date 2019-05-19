
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SQUARE_UF_H
#define SQUARE_UF_H
; 
float square_uf(float x){
    { return x*x; }; 
}

#endif
 ; 
void square(float * v_initial_param_855_368, float * & v_user_func_857_369, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_857_369 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_367 = 0;(v_i_367 <= (-1 + v_N_0)); (++v_i_367)){
        v_user_func_857_369[v_i_367] = square_uf(v_initial_param_855_368[v_i_367]); 
    }
}
}; 