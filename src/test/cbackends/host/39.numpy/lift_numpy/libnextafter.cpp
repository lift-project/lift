
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
void nextafter(float * v_initial_param_7512_3045, float * v_initial_param_7513_3046, float * & v_user_func_7519_3048, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7519_3048 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3044 = 0;(v_i_3044 <= (-1 + v_N_2763)); (++v_i_3044)){
        v_user_func_7519_3048[v_i_3044] = nextafter_uf(v_initial_param_7512_3045[v_i_3044], v_initial_param_7513_3046[v_i_3044]); 
    }
}
}; 