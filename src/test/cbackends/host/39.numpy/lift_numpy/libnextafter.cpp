
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
void nextafter(float * v_initial_param_540_262, float * v_initial_param_541_263, float * & v_user_func_547_265, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_547_265 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_261 = 0;(v_i_261 <= (-1 + v_N_0)); (++v_i_261)){
        v_user_func_547_265[v_i_261] = nextafter_uf(v_initial_param_540_262[v_i_261], v_initial_param_541_263[v_i_261]); 
    }
}
}; 