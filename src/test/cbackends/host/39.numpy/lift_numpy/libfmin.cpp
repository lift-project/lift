
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMIN_UF_H
#define FMIN_UF_H
; 
float fmin_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void fmin(float * v_initial_param_918_395, float * v_initial_param_919_396, float * & v_user_func_925_398, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_925_398 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_394 = 0;(v_i_394 <= (-1 + v_N_0)); (++v_i_394)){
        v_user_func_925_398[v_i_394] = fmin_uf(v_initial_param_918_395[v_i_394], v_initial_param_919_396[v_i_394]); 
    }
}
}; 