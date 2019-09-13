
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIVIDE_UF_H
#define DIVIDE_UF_H
; 
float divide_uf(float x, float y){
    return x / y;; 
}

#endif
 ; 
void true_divide(float * v_initial_param_7624_3084, float * v_initial_param_7625_3085, float * & v_user_func_7631_3087, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7631_3087 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3083 = 0;(v_i_3083 <= (-1 + v_N_2763)); (++v_i_3083)){
        v_user_func_7631_3087[v_i_3083] = divide_uf(v_initial_param_7624_3084[v_i_3083], v_initial_param_7625_3085[v_i_3083]); 
    }
}
}; 