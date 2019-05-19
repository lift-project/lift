
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
void nextafter(float * v_initial_param_540_264, float * v_initial_param_541_265, float * & v_user_func_547_267, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_547_267 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_263 = 0;(v_i_263 <= (-1 + v_N_0)); (++v_i_263)){
        v_user_func_547_267[v_i_263] = nextafter_uf(v_initial_param_540_264[v_i_263], v_initial_param_541_265[v_i_263]); 
    }
}
}; 