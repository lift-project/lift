
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
void nextafter(float * v_initial_param_3568_634, float * v_initial_param_3569_635, float * & v_user_func_3575_637, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3575_637 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_633 = 0;(v_i_633 <= (-1 + v_N_352)); (++v_i_633)){
        v_user_func_3575_637[v_i_633] = nextafter_uf(v_initial_param_3568_634[v_i_633], v_initial_param_3569_635[v_i_633]); 
    }
}
}; 