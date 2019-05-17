
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
void nextafter(float * v_initial_param_521_243, float * v_initial_param_522_244, float * & v_user_func_528_246, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_528_246 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_242 = 0;(v_i_242 <= (-1 + v_N_0)); (++v_i_242)){
        v_user_func_528_246[v_i_242] = nextafter_uf(v_initial_param_521_243[v_i_242], v_initial_param_522_244[v_i_242]); 
    }
}
}; 