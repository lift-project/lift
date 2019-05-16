
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
void nextafter(float * v_initial_param_515_237, float * v_initial_param_516_238, float * & v_user_func_522_240, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_522_240 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_236 = 0;(v_i_236 <= (-1 + v_N_0)); (++v_i_236)){
        v_user_func_522_240[v_i_236] = nextafter_uf(v_initial_param_515_237[v_i_236], v_initial_param_516_238[v_i_236]); 
    }
}
}; 