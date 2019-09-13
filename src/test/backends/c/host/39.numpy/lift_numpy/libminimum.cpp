
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MINIMUM_UF_H
#define MINIMUM_UF_H
; 
float minimum_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void minimum(float * v_initial_param_7862_3166, float * v_initial_param_7863_3167, float * & v_user_func_7869_3169, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7869_3169 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3165 = 0;(v_i_3165 <= (-1 + v_N_2763)); (++v_i_3165)){
        v_user_func_7869_3169[v_i_3165] = minimum_uf(v_initial_param_7862_3166[v_i_3165], v_initial_param_7863_3167[v_i_3165]); 
    }
}
}; 