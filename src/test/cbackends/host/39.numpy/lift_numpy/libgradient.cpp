
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef GRAD2_UF_H
#define GRAD2_UF_H
; 
float grad2_uf(float l, float r){
    { return (l - r)/2.0f; }; 
}

#endif
 ; 
void gradient(float * v_initial_param_3393_569, float * & v_user_func_3399_570, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3399_570 = reinterpret_cast<float *>(malloc(((-2 + v_N_352) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_568 = 0;(v_i_568 <= (-3 + v_N_352)); (++v_i_568)){
        v_user_func_3399_570[v_i_568] = grad2_uf(v_initial_param_3393_569[(2 + v_i_568)], v_initial_param_3393_569[v_i_568]); 
    }
}
}; 