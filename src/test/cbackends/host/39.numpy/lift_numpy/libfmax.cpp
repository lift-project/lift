
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMAX_UF_H
#define FMAX_UF_H
; 
float fmax_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void fmax(float * v_initial_param_904_390, float * v_initial_param_905_391, float * & v_user_func_911_393, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_911_393 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_389 = 0;(v_i_389 <= (-1 + v_N_0)); (++v_i_389)){
        v_user_func_911_393[v_i_389] = fmax_uf(v_initial_param_904_390[v_i_389], v_initial_param_905_391[v_i_389]); 
    }
}
}; 