
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
void nextafter(float * v_initial_param_529_251, float * v_initial_param_530_252, float * & v_user_func_536_254, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_536_254 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_250 = 0;(v_i_250 <= (-1 + v_N_0)); (++v_i_250)){
        v_user_func_536_254[v_i_250] = nextafter_uf(v_initial_param_529_251[v_i_250], v_initial_param_530_252[v_i_250]); 
    }
}
}; 