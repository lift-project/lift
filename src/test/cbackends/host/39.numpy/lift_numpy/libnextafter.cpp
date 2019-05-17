
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEXTAFTER_UF_H
#define NEXTAFTER_UF_H
; 
float nextafter_uf(float x, float y){
    return x<y? x+ std::numeric_limits<float>::epsilon() : (x>y? x - std::numeric_limits<float>::epsilon() : x)   ;; 
}

#endif
 ; 
void nextafter(float * v_initial_param_524_246, float * v_initial_param_525_247, float * & v_user_func_531_249, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_531_249 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_245 = 0;(v_i_245 <= (-1 + v_N_0)); (++v_i_245)){
        v_user_func_531_249[v_i_245] = nextafter_uf(v_initial_param_524_246[v_i_245], v_initial_param_525_247[v_i_245]); 
    }
}
}; 