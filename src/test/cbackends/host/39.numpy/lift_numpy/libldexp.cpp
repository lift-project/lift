
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LDEXP_UF_H
#define LDEXP_UF_H
; 
float ldexp_uf(float x, float y){
    return x* pow(2,y) ;; 
}

#endif
 ; 
void ldexp(float * v_initial_param_7561_3040, float * v_initial_param_7562_3041, float * & v_user_func_7568_3043, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7568_3043 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3039 = 0;(v_i_3039 <= (-1 + v_N_2763)); (++v_i_3039)){
        v_user_func_7568_3043[v_i_3039] = ldexp_uf(v_initial_param_7561_3040[v_i_3039], v_initial_param_7562_3041[v_i_3039]); 
    }
}
}; 