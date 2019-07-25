
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
void ldexp(float * v_initial_param_3617_629, float * v_initial_param_3618_630, float * & v_user_func_3624_632, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3624_632 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_628 = 0;(v_i_628 <= (-1 + v_N_352)); (++v_i_628)){
        v_user_func_3624_632[v_i_628] = ldexp_uf(v_initial_param_3617_629[v_i_628], v_initial_param_3618_630[v_i_628]); 
    }
}
}; 